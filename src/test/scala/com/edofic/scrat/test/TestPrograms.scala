package com.edofic.scrat.test

/**
 * User: andraz
 * Date: 2.9.12
 * Time: 12:32
 */
object TestPrograms {
  val tuples: List[(String, String, Any)] = List(
    ("math expressions", "(1+3)*5/2^(5-2)", (1 + 3) * 5 / math.pow(2, (5 - 2))),

    ("strings", "\"hi\"", "hi"),

    ("assignments",
      """
         a=1
         b=2
        |c=3
        |a+b+c
      """.stripMargin, 6),

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
      """.stripMargin, 1),

    ("equalites and inequalities",
      """
        |(1==0)+2*(1==1)+4*(1!=0)+8*(1!=1)+16*(3>2)+32*(2>3)+64*(1<2)+128*(2<1)+256*(1>=0)+512*(1>=1)+1024*(1>=2)+2048*(1<=0)+4096*(1<=1)+8192*(1<=2)
      """.stripMargin, 0 + 2 + 4 + 0 + 16 + 0 + 64 + 0 + 256 + 512 + 0 + 0 + 4096 + 8192),

    ("currying",
      """
        |func plus(a)(b){a+b}
        |addOne = plus(1)
        |addOne(1) * plus(2)(4)
      """.stripMargin, 12),

    ("syntax sugar for short functions",
      """
        |f = func(n) n+1
        |g = func(a)(b) a+b
        |h = func(a) func (b) a+b
        |
        |a = g(f(0))(0)*h(1)(1)
        |b = func(x){x+1}(0)
        |a*b
      """.stripMargin, 2),

    ("arrays",
      """
        |array=[1,2,3]
        |array(0) + array(1) + array(2)
      """.stripMargin, 6),

    ("if then else",
      """
        |if (1) then 1 else 2
      """.stripMargin, 1),

    ("fibonnaci numbers",
      """
        |func fib(n) if n<2 then 1 else fib(n-1) + fib(n-2)
        |fib(20)
      """.stripMargin, 10946),

    ("countdown / tail recursion",
      """
        |func count(n) if n<0 then n else count(n-1)
        |count(100000)
      """.stripMargin, -1)
  )
}
