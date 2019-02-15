package com.teningur.arbor


object Sample extends App {
  val global =
    vals"""
          x = ${10}
        """

  val plus = Plus(Plus(Ref("x"), Ref("y")), Ref("z"))

  val outer = CreateClosure(Discard(Bind("x", Num(5)), CreateClosure(plus)))
  println(Invoke(PartialApply(Invoke(outer, params""), params"z = ${"x"}"), params"y = ${3}").s.runA(global).value)

  """
      x = 10
      { z ->
        {
           x = 5
          { y →
            x + y + z
          }
        }()
      }(z = x)(y = 3)
  """.stripMargin
}