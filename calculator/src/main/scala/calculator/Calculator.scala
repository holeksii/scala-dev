package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
  import Expr.*

  private val visited = scala.util.DynamicVariable[Set[String]](Set.empty)

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]
  ): Map[String, Signal[Double]] =
    namedExpressions.map { case (name, exprSignal) =>
      name -> Signal {
        visited.withValue(Set.empty) {
          eval(exprSignal(), namedExpressions)
        }
      }
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using
      Signal.Caller
  ): Double =
    expr match
      case Literal(v) => v
      case Ref(name) =>
        if visited.value.contains(name) then Double.NaN
        else
          visited.value += name
          eval(getReferenceExpr(name, references), references)
      case Plus(a, b)  => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) =>
        val divisor = eval(b, references)
        if divisor == 0 then Double.NaN
        else eval(a, references) / divisor

  /** Get the Expr for a referenced variables. If the variable is not known,
    * returns a literal NaN.
    */
  private def getReferenceExpr(
      name: String,
      references: Map[String, Signal[Expr]]
  )(using Signal.Caller): Expr =
    references
      .get(name)
      .fold[Expr] {
        Literal(Double.NaN)
      } { exprSignal =>
        exprSignal()
      }
