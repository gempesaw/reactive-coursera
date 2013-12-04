package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("Future.all passes") {
    val fs = List(Future.always(1), Future.always(2))
    val all = Future.all(fs)
    val res = Await.result(all, 1 second)
    assert(res === List(1, 2))
  }

  test("Future.all fails") {
    val fs = List(Future.always(1), Future.failed(new Exception))
    val all = Future.all(fs)
    intercept[Exception](Await.result(all, 1 second))
  }

  test("Future.any when all pass") {
    val fs = List(Future { Thread.sleep(15); 8 }, Future { Thread.sleep(10); 5 } )
    val any = Future.any(fs)
    val res = Await.result(any, 1 second)
    assert(res === 5)
  }

  test("Future.any when first passes") {
    val fs = List(Future.never, Future { Thread.sleep(10); 5 } )
    val any = Future.any(fs)
    val res = Await.result(any, 1 second)
    assert(res === 5)
  }

  test("Future.any when first fails") {
    val fs = List(Future.failed(new Exception), Future { Thread.sleep(10); 5 } )
    val any = Future.any(fs)
    intercept[Exception](Await.result(any, 1 second))
  }

  test("A Future should be completed after 3s when using a delay of 1s") {
    Await.ready(Future.delay(1 second), 3 seconds)
    assert(true)
  }

  test("A Future should not complete after 1s when using a delay of 3s") {
    intercept[TimeoutException] {
      val future = Future.delay(3 seconds)
      Await.result(future, 1 second)
    }
  }

  test("Future.now completed") {
    val always = Future.always(5)
    assert(always.now === 5)
  }

  test("Future.now should throw an exception if it's not completed") {
    val never = Future.never
    intercept[NoSuchElementException](never.now)
  }

  test("Future.continueWith should contain the continued value") {
    val future = Future.always(1).continueWith { _ => "goodbye" }
    val res = Await.result(future, 1 second)
    assert(res === "goodbye")
  }

  test("Future.continueWith should handle exceptions thrown by the user specified continuation function") {
    val future = Future.always("hello").continueWith { _ => throw new Exception }
    intercept[Exception](Await.result(future, 1 second))
  }

  test("continueWith should handle exceptions thrown by f, the original future") {
    val future = Future { throw new Exception } continueWith { _ => "goodbye" }
    intercept[Exception](Await.result(future, 1 second))
  }

  test(" Future.continue should contain the continued value") {
    val future = Future.always(1).continue { _ => "goodbye" }
    val res = Await.result(future, 1 second)
    assert(res === "goodbye")
  }

  test("Future.continue should handle exceptions thrown by the user specified continuation function") {
    val future = Future.always("hello").continue { _ => throw new Exception }
    intercept[Exception](Await.result(future, 1 second))
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("Future.run unsubscribes when it's done") {
    var finished = false
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          // println("working")
        }
        // println("done")
        finished = true
      }
    }
    Future.delay(100 microseconds) onSuccess {
      case _ => working.unsubscribe()
    }

    Await.ready(Future.delay(500 microseconds), Duration.Inf)
    assert(finished)
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}
