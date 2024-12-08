enum Op:
	case Add
	case Mul
	case Concat

val Part1Ops = Seq(Op.Add, Op.Mul)
val Part2Ops = Seq(Op.Add, Op.Mul, Op.Concat)

// Recursive version using idea from
// https://github.com/OzairFaizan/AdventOfCode/tree/502275793b30d2c864c3cd03f77059b7e9f4125c/2024/day07
def isTargetable(numbers: Seq[Long], ops: Seq[Op], target: Long) =
	def aux(revNumbers: List[Long], target: Long): Boolean = revNumbers match
		case Nil => false
		case head :: Nil => head == target
		case head :: tail => ops.exists(op =>
			val (targetStr, headStr) = (target.toString, head.toString)
			op match
				case Op.Add if target - head >= 0 => aux(tail, target - head)
				case Op.Mul if target % head == 0 => aux(tail, target / head)
				case Op.Concat if targetStr.length - headStr.length > 0 && targetStr.endsWith(headStr) =>
					aux(tail, targetStr.dropRight(headStr.length).toLong)
				case _ => false
		)

	aux(numbers.reverse.toList, target)

scala.io.Source.fromFile("inputs/day7.txt").getLines()
	.map(l =>
		val split = l.split(": ")
		val target = split(0).toLong
		val numbers = split(1).split(" ").map(_.toLong)
		(numbers.toList, target)
	).toList
	.filter((numbers, target) => isTargetable(numbers.toSeq, Part2Ops, target))
	.map(_._2)
	.sum