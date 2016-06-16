package recfun

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
    if (c == 0 || r == c) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanced(notClosed: Int, rest: List[Char]): Boolean = {
      if (rest.isEmpty) {
        notClosed == 0
      } else {
        val head = rest.head
        if (head == ')') {
          if (notClosed > 0) balanced(notClosed - 1, rest.tail) else false
        }
        else if (head == '(') balanced(notClosed+1, rest.tail)
        else balanced(notClosed, rest.tail)
      }
    }
    balanced(0, chars);
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else {
      if (money - coins.head > 0) countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else if (money - coins.head == 0) 1 + countChange(money, coins.tail)
      else countChange(money, coins.tail)
    }
  }
}