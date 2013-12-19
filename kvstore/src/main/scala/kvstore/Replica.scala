package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import scala.concurrent.duration.Duration
import akka.actor.Props
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import akka.actor.UntypedActor
import akka.actor.Cancellable
import language.postfixOps
import scala.concurrent.{ Future, ExecutionContext }
import akka.util.Timeout
import scala.util.matching.Regex
import scala.collection.mutable.Map

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation
  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher
  import akka.actor.UntypedActor
  import akka.actor.Cancellable
  case class Pending(
    key: String,
    value: Option[String],
    sender: ActorRef,
    attempts: Int,
    retry: Cancellable,
    pendingReplicators: Set[ActorRef],
    timeout: Cancellable
  )
  case class PersistAttempt(key: String, value: Option[String], id: Long, msg: String)
  val system = akka.actor.ActorSystem("system")

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var updates = Map.empty[Long, Pending]

  def createPersistence: ActorRef = context.actorOf(persistenceProps)
  var persistence = createPersistence

  arbiter ! Join

  var rank = ""
  def receive = {
    case JoinedPrimary   => rank = "leader"; context.become(leader)
    case JoinedSecondary => rank = "replica"; context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case msg @ Insert(key, value, id) => persistAttempt(key, Option(value), id, msg.toString)
    case PersistAttempt(key, value, id, msg) => persistAttempt(key, value, id, msg)
    case Persisted(key, id) => updateKv(key, id)
    case Replicated(key, id) => replicatedOnSecondary(key, id)
    case msg @ Remove(key, id) => persistAttempt(key, None, id, msg.toString)
    case OperationFailed(id) => notifyAboutFailedOp(id)
    case Replicas(r) => manageSecondaries(r - self)
    case Get(key, id) => get(key, id)
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case msg @ Snapshot(key, value, seq) => checkSnapshotSeq(key, value, seq, msg.toString)
    case PersistAttempt(key, value, id, msg) => persistAttempt(key, value, id, msg)
    case Persisted(key, id) => updateKv(key, id)
    case Get(key, id) => get(key, id)
  }

  def replicatedOnSecondary(key: String, id: Long): Unit = {
    val p = updates(id)
    updates(id) = p.copy(pendingReplicators = p.pendingReplicators - sender)
    self ! Persisted(key, id)
  }

  def manageSecondaries(secs: Set[ActorRef]): Unit = {
    val deletedSecs = secondaries.keySet.diff(secs)
    val newSecs = secs.diff(secondaries.keySet)

    deletedSecs foreach { deleted =>
      val r = secondaries(deleted)
      r ! PoisonPill
      secondaries -= deleted
      replicators -= r

      val temp = Map.empty[Long, Pending]
      updates foreach {
        case (id, p) => {
          if (p.pendingReplicators.nonEmpty) {
            temp(id) = p.copy(pendingReplicators = p.pendingReplicators - r)
          }
        }
      }

      temp foreach { case (id, p) => updates(id) = p; self ! Persisted(p.key, id) }
    }

    newSecs foreach { s =>
      val replicator = context.actorOf(Replicator.props(s))
      secondaries += ((s, replicator))
      replicators += replicator

      for {
        (r, index) <- replicators.zipWithIndex
        (k, v) <- kv
      } yield r ! Replicate(k, Option(v), index)
    }
  }

  def notifyAboutFailedOp(id: Long): Unit = {
    val failed = updates(id)

    failed.retry.cancel
    failed.sender ! OperationFailed(id)
  }

  var snapReqMap = Map.empty[Long, ActorRef]
  var previousSequences = List.empty[Long]
  def checkSnapshotSeq(key: String, value: Option[String], id: Long, msg: String): Unit = {
    if (previousSequences.isEmpty && id != 0) {
      // we don't want to send anything
    } else if ((previousSequences.isEmpty && id == 0) || (!previousSequences.contains(id) && previousSequences.forall(x => id > x) && id == previousSequences.head + 1)) {
      self ! PersistAttempt(key, value, id, msg)
      snapReqMap += ((id, sender))
      previousSequences = id :: previousSequences
    }
    else {
      // if we re-use the same id number, we immediately send an ack
      sender ! SnapshotAck(key, id)
    }
  }

  def persistAttempt(key: String, value: Option[String], id: Long, msg: String): Unit = {
    if (updates.contains(id) && updates(id).attempts >= 10) {
      self ! OperationFailed(id)
    }
    else {
      persistence ! Persist(key, value, id)

      val retryTimeout = system.scheduler.scheduleOnce(100 milliseconds) {
        self ! PersistAttempt(key, value, id, msg)
      }

      updates.get(id) match {
        case Some(p) => updates(id) = p.copy(attempts = p.attempts + 1).copy(retry = retryTimeout)
        case None => {
          val timeout = system.scheduler.scheduleOnce(1 second) { self ! OperationFailed(id) }
          updates += ((id, Pending(key, value, sender, 0, retryTimeout, replicators, timeout)))
        }

      }
    }

    if (updates(id).attempts == 0) {
      replicators foreach { _ ! Replicate(key, value, id) }
      value match {
        case Some(value) => kv += ((key, value))
        case None => kv -= key
      }
    }
  }

  def updateKv(key: String, id: Long): Unit = {
    if (updates.contains(id)) {
      val persisted = updates(id)
      if (persisted.pendingReplicators.isEmpty) {
        if (rank == "primary") updates -= id
        persisted.retry.cancel
        persisted.timeout.cancel

        // :(
        if (rank == "replica") { snapReqMap(id) ! SnapshotAck(key, id)}
        else persisted.sender ! OperationAck(id)
      }
    }
  }

  def get(key: String, id: Long): Unit = {
    sender ! GetResult(key, kv.get(key), id)
  }

}
