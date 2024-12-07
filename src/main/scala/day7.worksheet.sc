enum Op:
	case Add
	case Mul
	case Concat

extension (op: Op)
	def apply(a: Long, b: Long) = op match
		case Op.Add => a + b
		case Op.Mul => a * b
		case Op.Concat => (a.toString ++ b.toString).toLong

case class OpGen(opCount: Int) extends Iterator[Seq[Op]]:
	var current = (0 until opCount).map(_ => Op.Add).toArray
	var permutationIndex = 0
	val max = scala.math.pow(3d, opCount).toLong 
	def hasNext = permutationIndex < max
	def next() =
		permutationIndex = permutationIndex + 1
		val r = current.toSeq
		var currentIndex = current.length - 1
		while currentIndex >= 0 do
			val next = current(currentIndex) match
				case Op.Add => Op.Mul
				case Op.Mul => Op.Concat
				case Op.Concat => Op.Add
			
			current.update(currentIndex, next)
			if next != Op.Add then // We have no "overflow"
				return r	
			else
				currentIndex = currentIndex - 1
		r

def crunch(numbers: Seq[Long], ops: Seq[Op]) =
	numbers.tail
		.zip(ops)
		.foldLeft(numbers.head)(
			(r, op) => op._2.apply(r, op._1)
		)

def exaustiveSearch(numbers: Seq[Long], target: Long) =
	OpGen(numbers.length - 1).find(ops => crunch(numbers, ops) == target)

scala.io.Source.fromFile("inputs/day7.txt").getLines()
	.map(l =>
		val split = l.split(": ")
		val target = split(0).toLong
		val numbers = split(1).split(" ").map(_.toLong)
		(numbers, target)
	)
	.filter((nbs, tgt) => exaustiveSearch(nbs.toIndexedSeq, tgt).isDefined)
	.map(_._2)
	.sum

// exaustiveSearch(nbs, 3267)
