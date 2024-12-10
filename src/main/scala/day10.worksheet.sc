case class Vec(x: Int, y: Int)
case class Vertex(height: Int, visited: Boolean, rating: Int)
case class Res(score: Int, rating: Int)

def computeScore(of: Vec, in: Array[Array[Vertex]]): Res =
	val current = in(of.y)(of.x)
	if current.visited then return Res(0, current.rating)
	val res = if current.height == 9 then 
		Res(1, 1)
	else
		val (visitable, unvisitable) = Seq(
			of.copy(x = of.x - 1),
			of.copy(x = of.x + 1),
			of.copy(y = of.y - 1),
			of.copy(y = of.y + 1),
		).filter(p => // Check bounds
			p.x >= 0 && p.x < in.head.length &&
			p.y >= 0 && p.y < in.length	
		).filter(p => // Check height difference
			in(p.y)(p.x).height == current.height + 1
		).partition(p =>
			!in(p.y)(p.x).visited
		)

		// We take only the rating from the unvisitable vertices
		val unvisitableRating = unvisitable.map(v => in(v.y)(v.x).rating).sum
		visitable.map(v => computeScore(v, in))
			.foldLeft(Res(0, unvisitableRating))((acc, r) => Res(acc.score + r.score, acc.rating + r.rating))
	
	// Cache the rating / mark visited
	// no need to cache the score since visited vertices either lead to no trail
	// or to a trail with which the current path shares the trail end
	in(of.y).update(of.x, current.copy(visited = true, rating = res.rating))
	res

val g = io.Source.fromFile("inputs/day10.txt").getLines()
	.map(line => line.map(c =>
		val h = c.toInt - '0'
		Vertex(h, false, if h == 9 then 1 else 0)
	).toList
).toList

val trailHeads = g.zipWithIndex.flatMap((line, y) =>
	line.zipWithIndex.filter((v, x) =>
		v.height == 0
	).map((_, x) => Vec(x, y))
)

trailHeads.map(head =>
	val temp = g.map(_.toArray).toArray
	computeScore(head, temp)
).foldLeft(Res(0, 0))((acc, r) => Res(acc.score + r.score, acc.rating + r.rating))
