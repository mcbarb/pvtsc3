package reductions

import org.scalameter._
import common.parallel
import scala.math.min

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceIter(from: Int, until: Int, acc: Int) : Int = {
      if (from >= until || acc < 0) acc
      else if (chars(from) == '(') balanceIter(from + 1, until, acc + 1)
      else if (chars(from) == ')') balanceIter(from + 1, until, acc - 1)
      else balanceIter(from + 1, until, acc)
    }

    balanceIter(0, chars.length, 0) == 0
  }

  /** Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int, acc1: Int, acc2: Int) : (Int,Int) = {
      if (from >= until) (acc1, acc2)
      else if (chars(from) == '(') traverse(from + 1, until, acc1 + 1, acc2)
      else if (chars(from) == ')') traverse(from + 1, until, acc1 - 1, min(acc1-1,acc2))
      else traverse(from + 1, until, acc1, acc2)
    }

    def reduce(from: Int, until: Int) : (Int,Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (tupL, tupR) = parallel(reduce(from,mid), reduce(mid,until))
        (tupL._1 + tupR._1, min(tupL._2, tupL._1 + tupR._2))
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
