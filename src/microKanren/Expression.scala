package microKanren

trait Expression

trait ExpressionLiteral extends Expression
case class ExpressionInt(value: Int) extends ExpressionLiteral
case class ExpressionString(value: String) extends ExpressionLiteral
case class ExpressionVariable(value: String) extends ExpressionLiteral

trait ExpressionPair extends Expression
case class ExpressionNull() extends ExpressionPair
case class ExpressionCons(car: Expression, cdr: Expression) extends ExpressionPair

case class ExpressionApply(func: Expression, exps: Expression*) extends Expression
case class ExpressionCF(expr: Expression) extends Expression
case class ExpressionConj(exp1: Expression, exp2: Expression) extends Expression
case class ExpressionDefine(name: String, expr: Expression) extends Expression
case class ExpressionDisj(exp1: Expression, exp2: Expression) extends Expression
case class ExpressionEq(exp1: Expression, exp2: Expression) extends Expression
case class ExpressionLambda(names: Seq[String], expr: Expression) extends Expression
