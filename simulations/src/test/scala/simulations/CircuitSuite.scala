package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or ff")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or tf")

    in1.setSignal(false)
    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or ft")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or tt")
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or ff")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or tf")

    in1.setSignal(false)
    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or ft")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or tt")
  }

  test("demux0") {
    val in, out = new Wire
    demux(in, Nil, List(out))

    in.setSignal(false)
    run
    assert(out.getSignal === false, "0, [] => 0")

    in.setSignal(true)
    run
    assert(out.getSignal === true, "1, [] => 1")

  }

  test("demux 1-1-2") {
    val in, c, o1, o2 = new Wire
    demux(in, List(c), List(o2, o1))

    in.setSignal(false);
    c.setSignal(false);
    run
    assert(o1.getSignal === false, "0, [0] => 0, _")
    assert(o2.getSignal === false, "0, [0] => _, 0")

    c.setSignal(true);
    run
    assert(o1.getSignal === false, "0, [1] => 0, _")
    assert(o2.getSignal === false, "0, [1] => _, 0")

    in.setSignal(true);
    c.setSignal(false);
    run
    assert(o1.getSignal === true, "1, [0] => 1, _")
    assert(o2.getSignal === false, "1, [0] => _, 0")

    c.setSignal(true);
    run
    assert(o1.getSignal === false, "1, [1] => 0, _")
    assert(o2.getSignal === true, "1, [1] => _, 1")
  }

  test("demux 1-2-4") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    demux(in, List(c1, c0), List(o3, o2, o1, o0))

    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(false)
    run
    assert(o0.getSignal === false, "0, [0, 0] => 0...")
    assert(o1.getSignal === false, "0, [0, 0] => .0..")
    assert(o2.getSignal === false, "0, [0, 0] => ..0.")
    assert(o3.getSignal === false, "0, [0, 0] => ...0")

    c0.setSignal(true)
    run
    assert(o0.getSignal === false, "0, [1, 0] => 0...")
    assert(o1.getSignal === false, "0, [1, 0] => .0..")
    assert(o2.getSignal === false, "0, [1, 0] => ..0.")
    assert(o3.getSignal === false, "0, [1, 0] => ...0")

    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(o0.getSignal === false, "0, [0, 1] => 0...")
    assert(o1.getSignal === false, "0, [0, 1] => .0..")
    assert(o2.getSignal === false, "0, [0, 1] => ..0.")
    assert(o3.getSignal === false, "0, [0, 1] => ...0")

    in.setSignal(true)
    c1.setSignal(false)
    run
    assert(o0.getSignal === true,  "1, [0, 0] => 1...")
    assert(o1.getSignal === false, "1, [0, 0] => .0..")
    assert(o2.getSignal === false, "1, [0, 0] => ..0.")
    assert(o3.getSignal === false, "1, [0, 0] => ...0")

    c0.setSignal(true)
    run
    assert(o0.getSignal === false, "1, [1, 0] => 0...")
    assert(o1.getSignal === true,  "1, [1, 0] => .1..")
    assert(o2.getSignal === false, "1, [1, 0] => ..0.")
    assert(o3.getSignal === false, "1, [1, 0] => ...0")

    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(o0.getSignal === false, "1, [0, 1] => 0...")
    assert(o1.getSignal === false, "1, [0, 1] => .0..")
    assert(o2.getSignal === true,  "1, [0, 1] => ..1.")
    assert(o3.getSignal === false, "1, [0, 1] => ...0")

    c0.setSignal(true)
    run
    assert(o0.getSignal === false, "1, [1, 1] => 0...")
    assert(o1.getSignal === false, "1, [1, 1] => .0..")
    assert(o2.getSignal === false, "1, [1, 1] => ..0.")
    assert(o3.getSignal === true,  "1, [1, 1] => ...1")
  }
}
