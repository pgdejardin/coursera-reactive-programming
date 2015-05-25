package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (p: (Int,Int)) =>
    val h1 = insert(p._1, empty)
    val h2 = insert(p._2, h1)
    findMin(h2) == ord.min(p._1, p._2)
  }

  property("insert delete") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sorting") = forAll { h: H =>
    val list: List[Int] = heapToList(h)
    list.sorted == list
  }

  property("melding") = forAll { (heaps: (H, H)) =>
    if (!isEmpty(heaps._1) && !isEmpty(heaps._2)) {
      val m1 = findMin(heaps._1)
      val m2 = findMin(heaps._2)
      val melded = meld(heaps._1, heaps._2)
      val meldedMin = findMin(melded)
      meldedMin == m1 || meldedMin == m2
    } else true
  }

  property("melding and sorting") = forAll { (heaps: (H, H)) =>
    val l1 = heapToList(heaps._1)
    val l2 = heapToList(heaps._2)
    val melded = meld(heaps._1, heaps._2)
    val lMelded = heapToList(melded)
    (l1 ++ l2).sorted == lMelded
  }

  def heapToList(heap: H): List[Int] =
    if (isEmpty(heap)) Nil else findMin(heap) :: heapToList(deleteMin(heap))

  lazy val genHeap: Gen[H] =
    for {
      value <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
