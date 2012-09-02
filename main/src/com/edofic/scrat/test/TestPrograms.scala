package com.edofic.scrat.test

/**
 * User: andraz
 * Date: 2.9.12
 * Time: 12:32
 */
object TestPrograms {
  val tuples: List[Tuple3[String, String, Any]] = List(
    ("math expressions", "(1+3)*5/2^(5-2)", (1 + 3) * 5 / math.pow(2, (5 - 2))),

    ("strings", "\"hi\"", "hi"),

    ("assignments",
      """
        |a=log(1)
        |b=pi
        |c=3
        |a+b+c
      """.stripMargin, 3 + math.Pi),

    ("functions",
      """
        |func t(n) { n*2 }
        |t(4)
      """.stripMargin, 8),

    ("function values",
      """
        |f = func (n) { n*3 - 1 }
        |f(4)
      """.stripMargin, 11),

    ("multilinefunctions",
      """
        |f = func (a,b) {
        |   x = a+1
        |   y = b-1
        |   x*y
        |}
        |f(3,3)
      """.stripMargin, 8),

    ("constructors",
      """
        |func create(a,b) { this }
        |o = create(5,7)
        |o.b - o.a
      """.stripMargin, 2),

    ("recursion",
      """
        |func fact(n) { if n==1 then 1 else n*fact(n-1) }
        |fact(5)
      """.stripMargin, 120),

    ("recursion with nested functions", //this will be very relevant with tail call optimization
      """
        |func fact(n) {
        |   func f(i, acc) {
        |     if i==0 then acc else f(i-1, acc*i)
        |   }
        |   f(n, 1)
        |}
        |fact(5)
      """.stripMargin, 120),

    ("dotAccessRecFunctions",
      """
        |func object(){this}
        |o = object()
        |o.get = func (n) { if n==0 then n else o.get(n-1) }
        |o.get(2)
      """.stripMargin, 0 toDouble),

    ("higher order functions",
      """
        |func apply(n, f){ f(n) }
        |apply(2, func (x) {x*2} )
      """.stripMargin, 4)
  )


}
