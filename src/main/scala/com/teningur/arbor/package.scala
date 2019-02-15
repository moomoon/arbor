package com.teningur

import cats.data.State

package object arbor {
  type ExprS = State[Env, Val]

  implicit class ExprSFilter(exprS: ExprS) {
    def withFilter(predicate: Val ⇒ Boolean): ExprS = exprS
      .map(v ⇒ if (predicate(v)) v else throw new IllegalArgumentException(s"predicate failed on $v"))
  }

  private val label = "\\s*([a-zA-Z]\\w*)\\s*=\\s*".r
  private val token = "\\s*([a-zA-Z]\\w*)\\s*".r
  private val strlit = ".*'(.*)'.*".r

  private object Expression {
    def unapply(arg: Any): Option[Expr] = arg match {
      case i: Int ⇒ Some(Num(i))
      case token(r) ⇒ Some(Ref(r))
      case strlit(s) ⇒ Some(Str(s))
    }
  }

  implicit class EnvOps(stringContext: StringContext) {
    private def env(t: TypedEnv.Type, values: Any*): Env = t(stringContext.parts.zip(values).map {
      case (label(l), Expression(expr: Val)) ⇒ l → expr
    }: _*)

    def vals(values: Any*): Env = env(Env.vals, values: _*)

    def args(values: Any*): Env = env(Env.args, values: _*)

    def params(values: Any*): Map[String, Expr] = Map(stringContext.parts.zip(values).map {
      case (label(l), Expression(expr)) ⇒ l → expr
    }: _*)
  }

}
