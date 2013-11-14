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

  lazy val genHeap: Gen[H] = for {
  	a <- arbitrary[A]
  	h <- oneOf(value(empty), genHeap)
  } yield insert(a, h)
  
  lazy val genHeap2: Gen[H] = insert(16, insert(18, insert(12, insert(24, insert(64, empty))))) 

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back
  property("min of two") = forAll { (a: Int, b:Int) =>
    val h = insert(b, insert(a, empty))
    val min = findMin(h)
    if (a < b) a == min else b == min
  }
  
  property("all items of heap") = forAll (genHeap2){ h:H =>
   val m = findMin(h)
   findMin(deleteMin(h)) > m

  }

   property("all items of heap") = forAll { (a:Int, b:Int) =>
   val h = insert(b, insert(a, empty))
   if (a < b) findMin(deleteMin(h)) == b else findMin(deleteMin(h)) == a

  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty
  property("insert into empty and delete min") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  // //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima
   property("continually deleting should return sorted sequence") = forAll (genHeap){ (h:H) =>
     //println(s"test----${h.label}")
     def checkAll(h:H, acc:Boolean):Boolean = {
       val a = findMin(h)
       //println(a)
       val h1 = deleteMin(h)
       if (isEmpty(h1)) { acc }
       else {
         checkAll(h1, acc && (a <= findMin(h1)))
       }
     }
     checkAll(h, true)
   }

}
