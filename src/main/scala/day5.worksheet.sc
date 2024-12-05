val lines = scala.io.Source.fromFile("inputs/day5.txt").getLines()

// PART 1
val deps: Map[Int, List[Int]] = lines.takeWhile(!_.isBlank).map(d =>
	val s = d.split("\\|", 2)
	s(0).toInt -> s(1).toInt
).foldLeft(Map.empty)((acc, dep) =>
	acc.updatedWith(dep._2)(depsOpt =>
		depsOpt match
			case Some(l) => Some(dep._1 :: l)
			case None => Some(List(dep._1))
	)
) 

val updates = lines.map(_.split(",").map(_.toInt).toList).toList

def isGood(update: List[Int]): Boolean =
	// isGood if the tail isGood and the head doesn't depend on the tail
	update match
		case Nil => true
		case head :: tail =>
			val headDeps = deps.getOrElse(head, List.empty)
			isGood(tail) && tail.forall(
				!headDeps.contains(_)
			)

updates.map(update =>
	if isGood(update) then
		update(update.length / 2)
	else 0
).sum

// PART 2
val noGood = updates.filter(!isGood(_))

noGood.map(update =>
	// A < B <=> page A can be before B, i.e no rules say that B is a dependancy of A
	update.sortWith((a, b) =>
		deps.getOrElse(b, List.empty).contains(a)
	)
).map(update => update(update.length / 2)).sum