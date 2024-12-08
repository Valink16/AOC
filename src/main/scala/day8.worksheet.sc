import scala.concurrent.ExecutionContext.parasitic
import scala.reflect.internal.util.Position
case class Vec(x: Int, y: Int)

val lines = scala.io.Source.fromFile("inputs/day8.txt")
	.getLines().toSeq
val (w, h) = (lines.head.length, lines.length)

val radarPositions: Map[Char, List[Vec]] = lines.zipWithIndex
	.map((l, y) => l.zipWithIndex.map((c, x) => c -> Vec(x, y)))
	.flatten
	.filter(_._1.isLetterOrDigit)
	.foldLeft(Map.empty)((acc, mapping) =>
		acc.updatedWith(mapping._1)(l => l match
			case Some(l) => Some(mapping._2 :: l)
			case None => Some(List(mapping._2))
		)
	)

def part1AN(first: Vec, second: Vec) =
	val delta = Vec(second.x - first.x, second.y - first.y)
	Seq(
		Vec(first.x - delta.x, first.y - delta.y),
		Vec(second.x + delta.x, second.y + delta.y)
	)

def advanceInBounds(start: Vec, delta: Vec, w: Int, h: Int): List[Vec] =
	if start.x < 0 || start.x >= w || start.y < 0 || start.y >= h then Nil
	else start :: advanceInBounds(Vec(start.x + delta.x, start.y + delta.y), delta, w, h)

def part2AN(first: Vec, second: Vec) =
	val delta = Vec(second.x - first.x, second.y - first.y)
	advanceInBounds(first, Vec(-delta.x, -delta.y), w, h) ++ advanceInBounds(second, delta, w, h)

radarPositions
	.filter((freq, positions) => positions match // Faster than going down the entire list for .length
		case Nil | Vec :: Nil => false
		case _ => true
	).flatMap((freq, positions) => 
		positions.combinations(2).flatMap(pair =>
			pair match
				case first :: second :: _ => part2AN(first, second)
				case _ => throw Exception("Unreachable since we use combinations of 2")
		)
	).filter(pos => pos.x >= 0 && pos.x < w && pos.y >= 0 && pos.y < h)
	.toSet.size // Check for bounds