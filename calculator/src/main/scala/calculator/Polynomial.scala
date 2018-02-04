package calculator

import scala.math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Var(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(if (delta.apply() >= 0) Set((-b() + sqrt(delta())) / (2 * a()), (-b() - sqrt(delta())) / (2 * a())) else Set())
  }
}
