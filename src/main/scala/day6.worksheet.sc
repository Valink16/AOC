case class Vec(x: Int, y: Int)
extension (v: Vec)
	def +(o: Vec): Vec =
		Vec(v.x + o.x, v.y + o.y)
	
	def rotRight: Vec =
		// Rot matrix is [[0, 1]^T, [-1, 0]^T]
		Vec(-v.y, v.x)

case class Guard(pos: Vec, vel: Vec)

enum Cell:
	case Empty
	case Obst
	case Visited(direction: Vec) // Stores the direction taken last time for loop detection

case class State(grid: Vector[Vector[Cell]], guard: Guard, counter: Int):
	val (tl, br) = (Vec(0, 0), Vec(grid.head.length - 1, grid.length - 1)) // Inclusive bounds

extension (s: State)
	def inBounds(v: Vec): Boolean =
		v.x >= s.tl.x && v.x <= s.br.x && v.y >= s.tl.y && v.y <= s.br.y

	def updated(pos: Vec, cell: Cell): State =
		s.copy(grid = s.grid
			.updated(
				pos.y,
				s.grid(pos.y)
					.updated(pos.x, cell)
			)
		)

	def update: State =
		val (nextCounter, nextGrid) = s.grid(s.guard.pos.y)(s.guard.pos.x) match
			case Cell.Visited(_) =>
				// Don't count, don't update last visited direction
				// and pray that loop detection finds the potential loop with the next steps instead
				(s.counter, s.grid)
			case _ =>
				(
					s.counter + 1,
					s.grid.updated(
						s.guard.pos.y,
						s.grid(s.guard.pos.y).updated(
							s.guard.pos.x,
							Cell.Visited(s.guard.vel)
						)
					)
				)

		val nextPos = s.guard.pos + s.guard.vel
		val nextGuard = if s.inBounds(nextPos) && s.grid(nextPos.y)(nextPos.x) == Cell.Obst then
			// Guard turns right
			s.guard.copy(
				pos = s.guard.pos,
				vel = s.guard.vel.rotRight
			)
		else
			// We can keep moving forward
			s.guard.copy(pos = nextPos)

		State(nextGrid, nextGuard, nextCounter)

	def draw =
		s.grid.zipWithIndex.map((l, y) =>
			l.zipWithIndex.map((c, x) =>
				if Vec(x, y) == s.guard.pos then
					'^'
				else
					c match
						case Cell.Visited(_) => 'X'
						case Cell.Obst => '#'
						case Cell.Empty => '.'
			).mkString
		).mkString("\n")

	def loop: State =
		var mutS = s
		while mutS.inBounds(mutS.guard.pos) do
			mutS = mutS.update
		mutS

	def loopSteps: List[State] =
		var mutS = s
		var l: List[State] = List.empty
		while mutS.inBounds(mutS.guard.pos) do
			l = mutS :: l
			mutS = mutS.update
		l.reverse

	// Detect if this state results in a loop
	// We can be sure it's a loop if we come back to a cell with the same direction
	def isLoop: Boolean =
		require(!s.grid.flatten.exists(_ match
			case Cell.Visited(_) => true
			case _ => false
		))

		var mutS = s
		while true do
			if mutS.inBounds(mutS.guard.pos) then
				if mutS.grid(mutS.guard.pos.y)(mutS.guard.pos.x) == Cell.Visited(mutS.guard.vel) then
					return true
				mutS = mutS.update
			else
				return false
		false // Unreachable

var guardPos = Vec(0, 0)

val grid = scala.io.Source.fromFile("inputs/day6.txt")
	.getLines()
	.zipWithIndex.map((l, y) =>
		l.zipWithIndex.map((c, x) =>
			c match
				case '.' => Cell.Empty
				case '#' => Cell.Obst
				case '^' => guardPos = Vec(x, y); Cell.Empty
		).toVector
	).toVector

val s = State(grid, Guard(guardPos, Vec(0, -1)), 0)

// PART 1
s.loop.counter

// PART 2
s.loopSteps
	.map(step => step.guard.pos)
	.filter(pot => pot != s.guard.pos)
	.filter(pot =>
			s
				.updated(pot, Cell.Obst)
				.isLoop
	)
	.toSet.size