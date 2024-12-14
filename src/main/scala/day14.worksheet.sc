import scala.util.matching.Regex
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
case class Vec(x: Int, y: Int)
def inBounds(v: Vec, tl: Vec, size: Vec) =
	v.x >= tl.x && v.x < tl.x + size.x && v.y >= tl.y && v.y < tl.y + size.y


val (w, h) = (101, 103)
val duration = 100
val re = Regex("p=([\\d]+),([\\d]+) v=(-*[\\d]+),(-*[\\d]+)", "x", "y", "vx", "vy")

val inputData = io.Source.fromFile("inputs/day14.txt").getLines().toSeq

def computePositions(duration: Int) = inputData
	.map(l =>
		val m = re.findFirstMatchIn(l).get
		(Vec(m.group("x").toInt, m.group("y").toInt),
		 Vec(m.group("vx").toInt, m.group("vy").toInt))
	).map((p, v) => Vec(
			Math.floorMod(p.x + duration * v.x, w), Math.floorMod(p.y + duration * v.y, h))
		)
	.toSeq

val quadrantSize = Vec(w / 2, h / 2)
val finalPositions = computePositions(100)
val tl = finalPositions.filter(v => inBounds(v, Vec(0, 0), quadrantSize))
val tr = finalPositions.filter(v => inBounds(v, Vec(quadrantSize.x + 1, 0), quadrantSize))
val bl = finalPositions.filter(v => inBounds(v, Vec(0, quadrantSize.y + 1), quadrantSize))
val br = finalPositions.filter(v => inBounds(v, Vec(quadrantSize.x + 1, quadrantSize.y + 1), quadrantSize))

// Part 1
tl.length * tr.length * bl.length * br.length

def searchLoop: Int =
	val initial = computePositions(0)
	var currentD = 1
	while true do
		val t = computePositions(currentD)

		if t.zip(initial).forall((a, b) => a == b) then
			return currentD

		currentD = currentD + 1
	0

def computeMean(in: Seq[Vec]): (Double, Double) =
	val sum = in.foldLeft(Vec(0, 0))((acc, v) => Vec(acc.x + v.x, acc.y + v.y))
	(sum.x.toDouble / in.length.toDouble, sum.y.toDouble / in.length.toDouble)

// Using idea from r/adventofcode/comments/1he0asr/2024_day_14_part_2_why_have_fun_with_image/
def variance(in: Seq[Vec]) =
	val mean = computeMean(in)
	val t = in.foldLeft((0.0, 0.0))((acc, v) =>
		val (dx, dy) = (v.x - mean._1, v.y - mean._2)
		(acc._1 + dx * dx, acc._2 + dy * dy)
	)

	(t._1 / in.length, t._2 / in.length)

// searchLoop

// val data = (0 to 11000).map(d =>
// 	val g = Array.ofDim[Int](h, w)
// 	computePositions(d).foreach(p => g(p.y).update(p.x, g(p.y)(p.x) + 1))
// 	d.toString + ": \n" + g.map(l => l.map(c => if c != 0 then c.toString else " ").mkString).mkString("\n")
// ).mkString("\n\n")
// Files.write(Paths.get("outputs/day14.txt"), data.getBytes(StandardCharsets.UTF_8))

val data = (0 to 11000).map(d =>
	val v = variance(computePositions(d))
	v._1 + v._2
).zipWithIndex.sortBy(p => p._1)
