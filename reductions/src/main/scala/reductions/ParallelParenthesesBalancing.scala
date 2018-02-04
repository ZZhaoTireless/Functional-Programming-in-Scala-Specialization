package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

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

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    var balancing = 0

    chars
      .map( c =>
        if (c == '(') { balancing += 1; true }
        else {
          if (c == ')') { balancing -= 1; if (balancing < 0) false else true }
          else true
        }
      )
      .forall(b => b) && balancing == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, currentLeft: Int, currentRight: Int): (Int, Int) = {
      var left = 0
      var right = 0

      chars.slice(idx, until)
        .foreach(c => {
          if (c == '(') { left += 1 }
          if (c == ')') { if ( left > 0 ) left -= 1 else right += 1 }
        })

      (left, right)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if ( until - from <= threshold ) traverse(from, until, 0, 0)
      else {
        val middle = from + (until - from) / 2
        val ((ll, lr), (rl, rr)) = parallel(reduce(from, middle), reduce(middle, until))

        if (ll >= rr) (ll - rr + rl, lr) else (rl, lr + rr - ll)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }
  // For those who want more:
  // Prove that your reduction operator is associative!

}
