package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for(
      (k, v) <- namedExpressions
    ) yield k -> Signal(eval(v(), namedExpressions))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case e: Literal => e.v
      case e: Plus => eval(e.a, references) + eval(e.b, references)
      case e: Minus => eval(e.a, references) - eval(e.b, references)
      case e: Times => eval(e.a, references) * eval(e.b, references)
      case e: Divide => eval(e.a, references) / eval(e.b, references)
      case e: Ref => eval(getReferenceExpr(e.name, references), references - e.name)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
