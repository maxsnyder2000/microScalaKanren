package scalaKanren

import microKanren.*

implicit class IndentedString(string: String) {
    def indented: String = ("\n" + string).replaceAll("\n", "\n\t")
}

object Generator {
    def imports: String =
        "import scalaKanren.*\n"
    
    def variableName(string: String): String =
        string.replaceAll("-|/", "_")
    
    def generate(expr: Expression): String = {
        expr match {
            case ExpressionInt(i) => i.toString
            case ExpressionString(s) => "\"" + s + "\""
            case ExpressionVariable(v) => variableName(v)
            case ExpressionNull() => "Null()"
            case ExpressionCons(car, cdr) =>
                val s1 = generate(car)
                val s2 = generate(cdr)
                "Cons(" + s1.indented + "," + s2.indented + "\n)"
            case ExpressionApply(f, es: _*) =>
                val sf = generate(f)
                val ss = es.map(generate)
                "(" + sf + ")(" + ss.mkString(",\n").indented + "\n)"
            case ExpressionCF(ExpressionLambda(Seq(n), e)) =>
                val v = variableName(n)
                val s = generate(e)
                "call_fresh(" + ("(" + v + ") => {" + s.indented + "\n}").indented + "\n)"
            case ExpressionConj(e1, e2) =>
                val s1 = generate(e1)
                val s2 = generate(e2)
                "conj(" + s1.indented + "," + s2.indented + "\n)"
            case ExpressionDefine(n, e) =>
                val (ns, e2) = e match {
                    case l @ ExpressionLambda(_, _) => (Some(l.names), l.expr)
                    case _ => (None, e)
                }
                val v = variableName(n)
                val vs = ns.map(_.map(variableName).map(_ + ": Value").mkString(", "))
                val s = generate(e2)
                "\ndef " + v + "(" + vs.getOrElse("") + "): Goal = {" + s.indented + "\n}\n"
            case ExpressionDisj(e1, e2) =>
                val s1 = generate(e1)
                val s2 = generate(e2)
                "disj(" + s1.indented + "," + s2.indented + "\n)"
            case ExpressionEq(e1, e2) =>
                val s1 = generate(e1)
                val s2 = generate(e2)
                "eq(" + s1.indented + "," + s2.indented + "\n)"
            case ExpressionLambda(Seq(), e) =>
                val s = generate(e)
                "ImmatureStream(" + ("() => {" + s.indented + "\n}").indented + "\n)"
            case ExpressionLambda(ns, e) =>
                val vs = ns.map(variableName)
                val s = generate(e)
                "(" + vs.mkString(", ") + ") => {" + s.indented + "\n}"
        }
    }
}
