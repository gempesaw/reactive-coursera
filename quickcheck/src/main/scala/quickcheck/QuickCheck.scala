package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("two elements into an empty heap, findMin gives smaller elem") = forAll { (x: Int, y: Int) =>
    val heap = insert(x, insert(y, empty))
    findMin(heap) == math.min(x, y)
  }

  property("delete min of heap with one element heap is empty") = forAll { a: Int =>
    val heap = insert(a, empty)
    val afterDelete = deleteMin(heap)
    isEmpty(afterDelete)
  }

  property("using heaps for sorting") = forAll { heap: H =>
    def isHeapSorted(heap: H): Boolean = {
      def sortFromHeap(sorted: List[A], heap: H): List[A] = {
        val currentMin = findMin(heap)
        val smallerHeap = deleteMin(heap)
        if (isEmpty(smallerHeap))
          currentMin :: sorted
        else
          sortFromHeap(currentMin :: sorted, smallerHeap)
      }

      val xs = sortFromHeap(Nil, heap)
      (xs, xs.tail).zipped.forall(_ >= _)
    }

    isHeapSorted(heap)
  }

  property("min(x meld y) is either min(x) or min(y)") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    findMin(melded) == math.min(findMin(h1), findMin(h2))
  }

  property("arbitrary element and heap") = forAll { (a: Int, h: H) =>
    findMin(insert(a, h)) == math.min(a, findMin(h))
  }

  property("meld keeps all of both") = {
    val h1 = insert( 1, insert( 2, insert( 3, insert( 4, empty))))
    val h2 = insert( 5, insert( 6, insert( 7, insert( 8, empty))))
    val melded = meld(h2, h1)
    findMin(deleteMin(melded)) == 2
  }

  property("oddly melded lists are equal") = forAll { (h1: H, h2: H) =>
    def isHeapEqual(h1: H, h2: H): Boolean = {
      def isEqualIter( status: Boolean, h1: H, h2: H): Boolean = {
        if (isEmpty(h1))
          if (isEmpty(h2))
            true
          else
            false
        else
          status && isEqualIter(findMin(h1) == findMin(h2), deleteMin(h1), deleteMin(h2))
      }

      isEqualIter( true, h1, h2)
    }

    isHeapEqual(meld(deleteMin(h1), insert(findMin(h1), h2)), meld(h1, h2))
  }

  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(value(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- oneOf(empty, genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
