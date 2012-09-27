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
      """.stripMargin, 0.toDouble),

    ("higher order functions",
      """
        |func apply(n, f){ f(n) }
        |apply(2, func (x) {x*2} )
      """.stripMargin, 4),

    ("llist.scrat and some tests",
      """
        | func llist(head, tail) {
        |    func cons(e) {
        |        llist(e, this)
        |    }
        |
        |    func get(i) {
        |        if i==0 then head else tail.get(i-1)
        |    }
        |
        |    func iterate(f) {
        |        if head!=none then f(head) else {}
        |        if tail!=none then tail.iterate(f) else {}
        |        none
        |    }
        |
        |    func iterateRight(f) {
        |        if tail!=none then tail.iterateRight(f) else {}
        |        if head!=none then f(head) else {}
        |        none
        |    }
        |
        |    func filter(p) {
        |        obj = this
        |        nl = none
        |        iterateRight(func (e){
        |            if p(e) then obj.nl=llist(e, obj.nl) else {}
        |        })
        |        nl
        |    }
        |
        |    func map(f) {
        |        obj = this
        |        nl = none
        |        iterateRight(func (e){
        |            obj.nl = llist(f(e), obj.nl)
        |        })
        |        nl
        |    }
        |
        |    func fold(start, f){
        |        obj = this
        |        iterate(func (e){
        |            obj.start = f(obj.start, e)
        |        })
        |        start
        |    }
        |
        |    func foldRight(start, f){
        |        obj = this
        |        iterateRight(func (e){
        |            obj.start = f(obj.start, e)
        |        })
        |        start
        |    }
        |
        |    func reduce(f){
        |        tail.fold(head, f)
        |    }
        |
        |    func reduceRight(f){
        |        tail.foldRight(head, f)
        |    }
        |
        |    this
        |}
        |
        |lnil = llist(none, none)
        |l = llist(1, llist(2, llist(1, llist(2, llist(3, llist(1, llist(4, lnil)))))))
        |func sum(a,b){a+b}
        |l.reduce(sum) == l.reduceRight(sum)
      """.stripMargin, 1),

    ("privates",
      """
        |func private(n) {
        |    that = this
        |    func (){
        |        func get() { that.n }
        |        func set(v) { that.n = v }
        |        this
        |    }()
        |}
        |o = private(0)
        |o.set(1)
        |o.get()
      """.stripMargin, 1)
  )
}
