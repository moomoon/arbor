package com.teningur.arbor

import com.teningur.arbor.TypedEnv.{ArgEnv, Type, ValEnv}

sealed trait Env {
  def ref(label: String): Option[Val]

  def +(another: Env): Env
}

trait TypedEnv extends Env {
  self ⇒
  def t: TypedEnv.Type


  def append(typed: TypedEnv): TypedEnv = t.sort(this, typed)

  override def +(another: Env): Env = another match {
    case typed@Type(at) if at == t ⇒ append(typed)
    case typed@Type(_) ⇒ Env(this, typed)
    case Env.Empty ⇒ this
    case full: Env.Full ⇒ full.prepend(this)
  }
}

object TypedEnv {

  sealed trait Type {
    def sort(x: TypedEnv, y: TypedEnv): TypedEnv

    def apply(elems: (String, Val)*): TypedEnv = TypedEnv.Immutable(this, Map(elems: _*))
  }

  object Type {
    def unapply(arg: TypedEnv): Option[Type] = Some(arg.t)

  }

  case class Immutable(override val t: TypedEnv.Type, refs: Map[String, Val]) extends TypedEnv {
    override def ref(label: String): Option[Val] = refs.get(label)
  }

  object ValEnv extends Type {
    override def sort(x: TypedEnv, y: TypedEnv): TypedEnv = Cons(y, x)

    override def toString: String = "Vals"
  }

  object ArgEnv extends Type {
    override def sort(x: TypedEnv, y: TypedEnv): TypedEnv = Cons(x, y)

    override def toString: String = "Args"
  }

  case class Cons(x: TypedEnv, y: TypedEnv) extends TypedEnv {
    override def t: Type = x.t

    override def ref(label: String): Option[Val] = x.ref(label) orElse y.ref(label)

  }

}

object Env {
  def args: TypedEnv.ArgEnv.type = TypedEnv.ArgEnv

  def vals: TypedEnv.ValEnv.type = TypedEnv.ValEnv

  object Empty extends Env {
    override def ref(label: String): Option[Val] = None

    override def +(another: Env): Env = another
  }

  case class Full(args: TypedEnv, vals: TypedEnv) extends Env {

    override def ref(label: String): Option[Val] = args.ref(label) orElse vals.ref(label)

    def prepend(typed: TypedEnv): Full = typed match {
      case a@TypedEnv.Type(ArgEnv) ⇒ copy(args = a.append(args))
      case v@TypedEnv.Type(ValEnv) ⇒ copy(vals = v.append(vals))
    }

    override def +(another: Env): Env = another match {
      case Env.Empty ⇒ this
      case typed@TypedEnv.Type(ArgEnv) ⇒ copy(args = args append typed)
      case typed@TypedEnv.Type(ValEnv) ⇒ copy(vals = vals append typed)
      case Full(a, v) ⇒ Full(args = args append a, vals = vals append v)
    }
  }

  def apply(x: TypedEnv, y: TypedEnv): Env = (x.t, y.t) match {
    case (ArgEnv, ValEnv) ⇒ Full(x, y)
    case (ValEnv, ArgEnv) ⇒ Full(y, x)
    case (xt, yt) if xt == yt ⇒ TypedEnv.Cons(x, y)
  }
}
