package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val positive100Nums:Set = (x:Int) => x > 0 && x < 101
    val negative100Nums:Set = (x:Int) => x > -101 && x < 0
    val negativeNums:Set = (x:Int) => x < 0
    val positiveNums:Set = (x:Int) => x > 0
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only common elements") {
    new TestSets {
      val s = intersect(positiveNums, negativeNums)
      assert(!contains(s, 1))
      assert(!contains(s, -1))

      val s4 = intersect(_ % 2 == 0, _ % 3 == 0)
      assert(contains(s4, 6))
      assert(contains(s4, -12))
      assert(!contains(s4, 9))

    }
  }

  test("diff returns the difference of two sets") {
    new TestSets {
      val s = diff(positiveNums, _ % 2 == 0)
      assert(contains(s, 1))
      assert(contains(s, 33))
      assert(!contains(s, 2))
      assert(!contains(s, 44))

      val s4 = diff(_ % 2 == 0, _ % 3 == 0)
      assert(!contains(s4, 6))
      assert(!contains(s4, -12))
      assert(contains(s4, 8))
      assert(contains(s4, -14))

      val s5:Set = (x:Int) => x > 0 && x < 10
      val s6 = diff(s5, _ % 2 == 0)
      assert(FunSets.toString(s6) === "{1,3,5,7,9}")  

    }
  }

  

  test("filter returns the subset of `s` for which `p` holds.") {
    new TestSets {
      val s = filter(positiveNums, negativeNums)
      assert(!contains(s, 1))
      assert(!contains(s, -1))

      val s4 = filter(negativeNums, _ < -10)
      assert(contains(s4, -11))
      assert(contains(s4, -1000))
      assert(!contains(s4, 0))
      assert(!contains(s4, -10))
      assert(!contains(s4, -1))

    }
  }

  test("exists returns true if there is a bounded integer that statisfies p") {
    new TestSets {
      assert(exists(_ < -900, _ % 100 == 0))
      assert(exists(_ < -900, _ % 1000 == 0))
      assert(exists(_ < -900, _ <  1000))
      assert(!exists(_ < -900, _ <  -1950))
      assert(exists(_ < -900, _ <  -950))
      assert(exists(_ < -900, _ == -950))
    }
  }

  test("forall returns true if all bounded integers statisfy p") {
    new TestSets {
      
      assert(!forall((x:Int) => x % 2 == 0, (y:Int) => y % 4 == 0))
      assert(forall((x:Int) => x % 4 == 0, (y:Int) => y % 2 == 0))
      assert(!forall((x:Int) => x % 4 == 0, (y:Int) => y % 3 == 0))
      assert(forall((x:Int) => x > 1000, (y:Int) => y < -1))
      assert(forall((x:Int) => x < -1000, (y:Int) => y > 1))
    }
  }

  test("map returns a set transformed by applying `f` to each element of `s`") {
   new TestSets {
      val s4 = map((x:Int) => x > 0 && x < 5, (y:Int) => y * y)
      assert(contains(s4, 1))
      assert(contains(s4, 4))
      assert(contains(s4, 9))
      assert(contains(s4, 16))

      assert(!contains(s4, 2))
      assert(!contains(s4, 3))
      assert(!contains(s4, 25))    

      assert(FunSets.toString(s4) === "{1,4,9,16}")  
    } 
  }
}
