case class Vec(x: Int, y: Int)

val directions = List(
	Vec(1, 0),
	Vec(-1, 0),
	Vec(0, 1),
	Vec(0, -1),
	Vec(1, 1),
	Vec(1, -1),
	Vec(-1, 1),
	Vec(-1, -1),
)

val s = scala.io.Source.fromFile("inputs/day4.txt")

// Build 2D array
val array = s.getLines().map(s =>
	s.toCharArray()	
).toArray

def explore(array: Array[Array[Char]], from: Vec, direction: Vec, word: List[Char]): Boolean =
	word match
		case Nil => true
		case x :: xs => 
			if (from.x < 0 || from.x >= array.head.length || from.y < 0 || from.y >= array.length)
				false
			else
				array(from.y)(from.x) == x &&
				explore(array, Vec(from.x + direction.x, from.y + direction.y), direction, xs)

val froms = for y <- (0 until array.length)
	x <- (0 until array.head.length)
	yield Vec(x, y)

froms.map(f =>
	directions.count(d =>
		explore(array, f, d, "XMAS".toList)
	)
).reduce(_ + _)

val aFroms = froms.filter(f => array(f.y)(f.x) == 'A')

aFroms.map(f =>
	val tl = Vec(f.x - 1, f.y - 1)
	val bl = Vec(f.x - 1, f.y + 1)

	val r = (explore(array, tl, Vec(1, 1), "MAS".toList) || explore(array, tl, Vec(1, 1), "SAM".toList)) &&
	(explore(array, bl, Vec(1, -1), "MAS".toList) || explore(array, bl, Vec(1, -1), "SAM".toList))
	(f, r)
).count(_._2 == true)
