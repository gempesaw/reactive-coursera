package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._
import language.postfixOps
import scala.collection.mutable.Map


object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)
  case class Kill()

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  import context.dispatcher
  import akka.actor.UntypedActor
  import akka.actor.Cancellable
  val system = akka.actor.ActorSystem("system")

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var ids = Map.empty[Long, Long]
  var acks = Map.empty[Long, (ActorRef, Replicate, Cancellable)]

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case req @ Replicate(key, valueOption, id) => replicate(key, valueOption, id, req)
    case SnapshotAck(key, seq) => confirmReceipt(seq)
  }

  def confirmReceipt(seq: Long): Unit = {
    acks.get(seq) match {
      case Some(a) => {
        acks -= seq
        a._3.cancel
        a._1 ! Replicated(a._2.key, a._2.id)
      }
      case None => {
        println(seq, acks)
      }
    }
  }

  def replicate(key: String, valueOption: Option[String], id: Long, request: Replicate): Unit = {
    val retryTimeout = system.scheduler.scheduleOnce(200 milliseconds) {
      self ! Replicate(key, valueOption, id)
    }

    ids.get(id) match {
      case Some(seq) => {
        val a = acks(seq)
        acks(seq) = ((a._1, a._2, retryTimeout))
        replica ! Snapshot(key, valueOption, seq)
      }
      case None => {
        acks += _seqCounter -> ((sender, request, retryTimeout))
        ids += ((id, _seqCounter))
        replica ! Snapshot(key, valueOption, _seqCounter)
        nextSeq
      }
    }
  }
}
