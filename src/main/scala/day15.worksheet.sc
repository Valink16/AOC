case class Vec(x: Int, y: Int)

val s = io.Source.fromFile("inputs/day15test.txt").getLines()

val g = s.takeWhile(!_.isBlank)
	.map(_.toCharArray)
	.toArray

val directions = for
	l <- s
	c <- l
yield c match
	case '>' => Vec(1, 0)
	case '<' => Vec(-1, 0)
	case 'v' => Vec(0, 1)
	case '^' => Vec(0, -1)

var bot = {
	val y = g.indexWhere(_.contains('@')) 
	Vec(y, g(y).indexWhere(_ == '@'))
}

def move(from: Vec, delta: Vec): Boolean =
	val nextPos = Vec(from.x + delta.x, from.y + delta.y)
	val next = g(nextPos.y)(nextPos.x) 
	if next == '.' then
		g(nextPos.y).update(nextPos.x, g(from.y)(from.x))
		true
	else if next == '#' then
		false
	else
		if move(nextPos, delta) then
			g(nextPos.y).update(nextPos.x, g(from.y)(from.x))
			true
		else
			false

def traceGrid(g: Array[Array[Char]]) =
	g.map(l => l.mkString).mkString("\n")

directions.foreach(d =>
	if move(bot, d) then
		g(bot.y + d.y).update(bot.x + d.x, '@')
		g(bot.y).update(bot.x, '.')

		bot = Vec(bot.x + d.x, bot.y + d.y)
)

println(traceGrid(g))

val indexed = for
	(l, y) <- g.zipWithIndex
	(c, x) <- l.zipWithIndex
yield (c, Vec(x, y))

indexed.filter(_._1 == 'O').map((_, p) => p.y * 100 + p.x).sum
	