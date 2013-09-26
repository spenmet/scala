package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c > r) throw new java.lang.IllegalArgumentException("invalid argument")
    val pos  = (c, r)
    pos match {
      case (a,b) if (a==b) => 1
      case (0,_) => 1
      case (1,r) => r
      case (x,y) => pascal(x-1,y-1) + pascal(x,y-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def count(c:Int, chars:List[Char]):Int = {
      chars match {
        case Nil => c 
        case '('::tail => count(c+1, tail)
        case ')'::tail if (c==0) => count(c+1, tail)
        case ')'::tail if (c!=0) => count(c-1, tail)     
        case _::tail => count(c, tail)
      }
    } 
    val c = count(0, chars)
    (c == 0)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (money < 0 ) 0
        else {
          coins match {
            case Nil => 0
            case head::tail  => countChange(money, tail) + countChange(money-head, coins) 
            
          }
        }
  }


}
