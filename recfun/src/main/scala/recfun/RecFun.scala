package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    (c, r) match
      case (a, b) if a == b => 1
      case (0, _) => 1
      case (a, b) => pascal(a, b-1) + pascal(a-1, b-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def go(chars: List[Char], i: Int): Boolean =
      if i < 0 then
        return false
      chars.headOption match
        case None if i == 0 => true
        case Some(c) if c == '(' => go(chars.tail, i+1)
        case Some(c) if c == ')' => go(chars.tail, i-1)
        case Some(c) => go(chars.tail, i)
        case _ => false
        
    go(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    (money, coins.headOption) match
      case (0, _) => 1
      case (_, None) => 0
      case _ =>
        val p = coins.filter(money < _)
        (0 to money/coins.head).map{ i =>
          val t = money - i * coins.head
          countChange(t, coins.tail)
        }.sum
    
