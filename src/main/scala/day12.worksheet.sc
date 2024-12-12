import scala.collection.mutable.Queue
case class Vertex(plant: Char, visited: Boolean)
case class Vec(x: Int, y: Int)
case class Res(area: Int, perimeter: Int, otherPlots: List[Vec])

val v = Vertex('X', false)

val original_g: IndexedSeq[IndexedSeq[Vertex]] = io.Source.fromFile("inputs/day12test.txt").getLines().map(line =>	
	line.map(c => Vertex(c, false)).toIndexedSeq
).toIndexedSeq

object Part1:
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

	def solve =
		val g = original_g.map(_.toArray).toArray
		val m = collection.mutable.Map.empty[Char, List[(Int, Int)]]
		var plotPositions = List.empty[Vec] // Store distinct plot group positions for part 2
		val toExplore = Queue(Vec(0, 0))
		while toExplore.length > 0 do
			val v = toExplore.dequeue()
			dfs(g, v) match
				case Res(area, perimeter, otherPlots) if area > 0 =>
					m.updateWith(g(v.y)(v.x).plant)(o => o match
						case None => Some(List((area, perimeter)))
						case Some(v) => Some((area, perimeter) :: v) 
					)
					plotPositions = v :: plotPositions
					toExplore.addAll(otherPlots.toSet)
				case _ => ()

		(m.values.map(v => v.foldLeft(0)((acc, v) => acc + v._1 * v._2)).sum, plotPositions)

object Part2:
	// BFS to find border -> Loop following it while counting the sides
	def bfs(g: Array[Array[Vertex]], from: Vec): Vec =
		val q = Queue(from)
		while !q.isEmpty do
			val v = q.dequeue()
			println(q)
			val currentVertex = g(v.y)(v.x)

			def inBounds = (v: Vec) => v.x >= 0 && v.x < g.head.length && v.y >= 0 && v.y < g.length
			val neighborPlots = Seq(
				Vec(v.x + 1, v.y),
				Vec(v.x - 1, v.y),
				Vec(v.x, v.y + 1),
				Vec(v.x, v.y - 1),
			)

			if neighborPlots.exists(v => !inBounds(v)) then
				return v
			
			if neighborPlots
				.map(v => g(v.y)(v.x))
				.exists(v => v.plant != currentVertex.plant) then
				return v

			val toQueue = neighborPlots
				.filter(v => inBounds(v) && !g(v.y)(v.x).visited && g(v.y)(v.x).plant == currentVertex.plant)
			
			toQueue.foreach(v => g(v.y).update(v.x, g(v.y)(v.x).copy(visited = true)))
			q.addAll(toQueue)
		throw Exception("Unreachable")

	def followBorder(g: Array[Array[Vertex]], from: Vec): Int =
		

val (_, plotPositions) = Part1.solve

val g2 = original_g.map(_.toArray).toArray
Part2.bfs(g2, Vec(1, 1))

