package com.teningur.arbor

import cats.data.State

trait Expr {
  def s: ExprS
}

sealed trait Val extends Expr {
  override def s: ExprS = State.pure(this)
}

sealed case class Closure(expr: Expr, env: Env) extends Val {
  def cs: ExprS = for {
    _ ← State.modify(env + _)
    v ← expr match {
      case c: Closure ⇒ c.cs
      case e ⇒ e.s
    }
  } yield v
}

case class Invoke(c: Expr, a: ParamList) extends Expr {
  override def s: ExprS = for {
    closure@Closure(_, _) ← c.s
    args@Arguments(_) ← a.s
    restore ← State.get[Env]
    _ ← State.set[Env](args.env)
    v ← closure.cs
    _ ← State.set(restore)
  } yield v
}

case class Decl(label: String, expr: Expr) extends Expr {
  override def s: ExprS = for {
    r ← expr.s
    env ← State.get[Env]
    _ ← State.set(env + Env.vals(label → r))
  } yield AUnit
}

case class PartialApply(expr: Expr, a: ParamList) extends Expr {
  override def s: ExprS = for {
    c@Closure(_, _) ← expr.s
    args@Arguments(_) ← a.s
  } yield Closure(c, args.env)
}

case class CreateClosure(expr: Expr) extends Expr {
  override def s: ExprS = for {
    state ← State.get[Env]
  } yield Closure(expr, state)
}

case class Discard(l: Expr, r: Expr) extends Expr {
  override def s: ExprS = for {
    _ ← l.s
    ret ← r.s
  } yield ret
}

case class Plus(l: Expr, r: Expr) extends Expr {
  override def s: ExprS = for {
    Num(ln) ← l.s
    Num(rn) ← r.s
  } yield Num(ln + rn)
}

case class Cond(cond: Expr, then: Expr, orElse: Expr) extends Expr {
  override def s: ExprS = for {
    Bool(b) ← cond.s
    expr = if (b) then else orElse
    r ← expr.s
  } yield r
}

case class Ref(r: String) extends Expr {
  override def s: State[Env, Val] =
    State.inspect { env ⇒
      env.ref(r).getOrElse(throw new IllegalArgumentException(s"Could not ref $r"))
    }
}

case class Num(n: Int) extends Val

case class Str(str: String) extends Val

case class Obj() extends Val

case class Bool(b: Boolean) extends Val

case object AUnit extends Val

class Arr() extends Val

case class ParamList(params: Map[String, Expr]) extends Expr {

  override def s: State[Env, Val] = params.foldLeft[Env State Map[String, Val]](State.pure(Map.empty)) {
    case (s, (label, expr)) ⇒
      for {
      m ← s
      v ← expr.s
    } yield m + (label → v)
  } map Arguments
}

object ParamList {
  def apply(params: (String, Expr)*): ParamList = ParamList(Map(params: _*))
}

case class Arguments(args: Map[String, Val]) extends Val {
  def env: TypedEnv = Env.args(args.toSeq: _*)
}