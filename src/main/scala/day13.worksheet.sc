import scala.util.matching.Regex
val deltaRe = Regex("X\\+([\\d]+), Y\\+([\\d]+)")
val priceRe = Regex("X=([\\d]+), Y=([\\d]+)")
val delta = 10000000000000L

case class Vec(x: Long, y: Long)

def solve(v1: Vec, v2: Vec, y: Vec): Option[Vec] =
	val det = v1.x * v2.y - v2.x * v1.y
	val xNum = Vec(v2.y * y.x - v2.x * y.y, -v1.y * y.x + v1.x * y.y)

	if det != 0 && xNum.x % det == 0 && xNum.y % det == 0 then 
		Some(Vec(xNum.x / det, xNum.y / det))
	else
		None

io.Source.fromFile("inputs/day13.txt").getLines()
	.grouped(4)
	.map(p => p.takeWhile(l => !l.isBlank()).toSeq)
	.map(p =>
		val v1Match = deltaRe.findFirstMatchIn(p(0)).get
		val v2Match = deltaRe.findFirstMatchIn(p(1)).get
		val priceMatch = priceRe.findFirstMatchIn(p(2)).get
		val v1 = Vec(v1Match.group(1).toInt, v1Match.group(2).toInt)
		val v2 = Vec(v2Match.group(1).toInt, v2Match.group(2).toInt)
		val price = Vec(priceMatch.group(1).toInt, priceMatch.group(2).toInt)

		(v1, v2, price)
	).map((v1, v2, y) => solve(v1, v2, Vec(y.x + delta, y.y + delta)))
	.filter(_.isDefined)
	.map(_.get)
	.map(v => v.x * 3 + v.y)
	.sum
