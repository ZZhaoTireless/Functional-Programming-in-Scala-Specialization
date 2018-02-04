package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    g <- frequency((1, const(empty)), (3, genHeap))
  } yield insert(i, g)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (x: A, y: A) =>
    findMin(insert(x, insert(y, empty))) == min(x, y)
  }

  property("gen3") = forAll { (m : A) =>
    deleteMin(insert(m, empty)) == empty
  }

  property("gen4") = forAll { (h: H) =>
    def recursiveDeleting(h: H, e: => A): Boolean = {
      if (isEmpty(h)) true else { if (findMin(h) < e) false else recursiveDeleting (deleteMin(h), findMin(h)) }
    }
    recursiveDeleting(h, findMin(h))
  }

  property("gen5") = forAll { (x: H, y: H) =>
    val meldMin = findMin(meld(x, y))
    meldMin == findMin(x) ||  meldMin == findMin(y)
  }

  property("gen6") = forAll { (x: H, y: H, z: H) =>
    def equal(x: H, y: H): Boolean = {
      if (isEmpty(x) && isEmpty(y)) true else { if (findMin(x) == findMin(y)) equal(deleteMin(x), deleteMin(y)) else false}
    }
    equal(meld(meld(x, y), z), meld(x, meld(y, z))) && equal(x, meld(empty, x))
  }

}
