val source = scala.io.Source.fromFile("inputs/day2.txt")

def isSafeDampened(input: Array[Int]) =	
	input.zipWithIndex.map((v, i) => input.patch(i, Nil, 1))
		.exists(isSafe)

def isSafe(input: Array[Int]): Boolean =
	val sign_and_distance = input
		.zip(input.drop(1))
		.map((a, b) => ((b - a).sign, (b - a).abs))

	sign_and_distance.forall((sign, distance) =>
		sign == sign_and_distance.head._1 &&
		distance >= 1 &&
		distance <= 3
	)

val input = source.getLines().map(_.split(' ').map(_.toInt))

input.map(isSafeDampened).toList.count(_ == true)
