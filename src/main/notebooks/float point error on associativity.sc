def f(u: Double, v: Double): Double =
  (u + v)/(1.0 + u*v)
def err(lst:List[Double]): Double =
  lst.reduceLeft(f) - lst.reduceRight(f)
def testAssoc: Double = {
  val r = new scala.util.Random
  val lst = List.fill(400)(r.nextDouble*0.002)
  err(lst)
}



val res = for (n <- 1 to 200) yield testAssoc

res.reduceLeft(_ min _)
res.reduceLeft(_ max _)

val length = 10
val chars = new Array[Char](length)