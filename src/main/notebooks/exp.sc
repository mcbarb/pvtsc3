import scalashop.Img

val teste = Seq((1,2), (2,3),(4,5))

teste.foldLeft((0, 0)) { case ((accA, accB), (a, b)) => (accA + a, accB + b) }


def groupByAndAvg[T, U](ts: Iterable[(T, U)])(implicit num: Numeric[U]) = {
  ts.groupBy(_._1).map {
    case (key, pairs) =>
      val values = pairs.map(_._2)
      key -> (num.toDouble(values.sum) / values.size)
  }
}

//teste unzip match { case (rr,gr) => map (num.toDouble(_.sum) / _.size)

(Seq(1,1,1),Seq(2,2,2)) match { case (a,b) => a.sum + b.sum}

Seq(1,3,5)

1 / 5
2 / 5
3 / 5
4 / 5
5 / 5
6 / 5

for (t <- 1 to 5) {
  println((t - 1) * 34 / 5
  , t * 34 / 5 )
}


val width = 45

//++ Range(width)

//val tips = (0 to 32).by(32/3)
//val tupleTips = tips zip tips.tail :+ 32

val numTasks = 42
val src = new Img(42, 42)
val tupleTips = for (t<-1 to numTasks) yield ((t - 1) * src.height / numTasks, t * src.height / numTasks )


tupleTips.length
