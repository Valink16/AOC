import scala.collection.mutable.Queue
case class Vertex(plant: Char, visited: Boolean)
case class Vec(x: Int, y: Int)
case class Res(area: Int, perimeter: Int, otherPlots: List[Vec])

val v = Vertex('X', false)

val original_g: IndexedSeq[IndexedSeq[Vertex]] = io.Source.fromFile("inputs/day12.txt").getLines().map(line =>	
	line.map(c => Vertex(c, false)).toIndexedSeq
).toIndexedSeq

// Returns the area, perimeter of a connected group of plots
// and the positions of all plots containing another plant
def dfs(g: Array[Array[Vertex]], from: Vec): Res =
	val currentVertex = g(from.y)(from.x)
	if currentVertex.visited then return Res(0, 0, List.empty)

	g(from.y).update(from.x, currentVertex.copy(visited = true))
	val (samePlots, otherPlots) = Seq(
		Vec(from.x + 1, from.y),
		Vec(from.x - 1, from.y),
		Vec(from.x, from.y + 1),
		Vec(from.x, from.y - 1),
	).filter(v => v.x >= 0 && v.x < g.head.length && v.y >= 0 && v.y < g.length)
	.partition(v => g(v.y)(v.x).plant == currentVertex.plant)

	samePlots
		.foldLeft(Res(1, 4 - samePlots.length, otherPlots.toList))((acc, neighborPos) =>
		dfs(g, neighborPos) match
			case Res(area, perimeter, otherPlots) => 
				Res(acc.area + area, acc.perimeter + perimeter, otherPlots ++ acc.otherPlots)
	)

// Returns the area, sides of a connected group of plots
// and the positions of all plots containing another plant
def dfsSides(g: Array[Array[Vertex]], from: Vec): Res =
	val currentVertex = g(from.y)(from.x)
	if currentVertex.visited then 
		return Res(0, 0, List.empty)

	g(from.y).update(from.x, currentVertex.copy(visited = true))

	val neighbors = Seq(
		Vec(from.x, from.y - 1), // TOP
		Vec(from.x + 1, from.y), // RIGHT
		Vec(from.x, from.y + 1), // BOTTOM
		Vec(from.x - 1, from.y), // LEFT
	)

	val crossNeighbors = Seq(
		Vec(from.x - 1, from.y - 1), // TOP LEFT
		Vec(from.x + 1, from.y - 1), // TOP RIGHT
		Vec(from.x + 1, from.y + 1), // BOTTOM RIGHT
		Vec(from.x - 1, from.y + 1), // BOTTOM LEFT
	)

	val (top, right, bottom, left, tl, tr, br, bl) = (neighbors ++ crossNeighbors).map(v =>
		v.x >= 0 && v.x < g.head.length && v.y >= 0 && v.y < g.length &&
		(g(v.y)(v.x).plant == currentVertex.plant) // true iff cell is part of the same plot
	) match {
		case Seq(top, right, bottom, left, tl, tr, br, bl) => (top, right, bottom, left, tl, tr, br, bl)
	}
	// TOP or RIGHT => no corner on the top right
	// BOTTOM or RIGHT => no corner on the bottom right
	// BOTTOM or LEFT => no corner on the bottom left
	// TOP or LEFT => no corner on the top left ==> De Morgan for when there is a corner
	// TOP, RIGHT and !TOP RIGHT => inner corner on the top right
	// TOP, LEFT and !TOP LEFT => inner corner on the top left
	// BOTTOM, RIGHT and !BOTTOM RIGHT => inner corner on the bottom right
	// BOTTOM, LEFT and !BOTTOM LEFT => inner corner on the bottom left	
	val corners = Seq(
		!top && !right,
		!bottom && !right,
		!bottom && !left,
		!top && !left,
		left && top && !tl,
		top && right && !tr,
		right && bottom && !br,
		bottom && left && !bl,
	).count(_ == true)

	val (samePlots, otherPlots) = neighbors
		.filter(v => v.x >= 0 && v.x < g.head.length && v.y >= 0 && v.y < g.length)
		.partition(v => g(v.y)(v.x).plant == currentVertex.plant)

	samePlots
		.filter(v => !g(v.y)(v.x).visited)
		.foldLeft(Res(1, corners, otherPlots.toList))((acc, neighborPos) =>
			dfsSides(g, neighborPos) match
				case Res(area, corners, otherPlots) =>
					Res(acc.area + area, acc.perimeter + corners, otherPlots ++ acc.otherPlots)
	)

def solve =
	val g = original_g.map(_.toArray).toArray
	val m = collection.mutable.Map.empty[Char, List[(Int, Int)]]
	val toExplore = Queue(Vec(0, 0))
	while toExplore.length > 0 do
		val v = toExplore.dequeue()
		dfsSides(g, v) match
			case Res(area, perimeter, otherPlots) if area > 0 =>
				m.updateWith(g(v.y)(v.x).plant)(o => o match
					case None => Some(List((area, perimeter)))
					case Some(v) => Some((area, perimeter) :: v) 
				)
				toExplore.addAll(otherPlots.toSet)
			case _ => ()

	m.values.map(v => v.foldLeft(0)((acc, v) => acc + v._1 * v._2)).sum

val g2 = original_g.map(_.toArray).toArray

solve
