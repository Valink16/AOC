val stones = io.Source.fromFile("inputs/day11.txt").getLines.next
	.split(" ")
	.map(_.toLong)
	.toList

import collection.mutable.Map
def countStones(stone: Long, steps: Int, cache: Vector[Map[Long, Long]]): Long =
	cache(steps).get(stone) match
		case Some(value) => return cache(steps).get(stone).get
		case _ =>
			val r = if steps == 0 then 1
			else
				val str = stone.toString
				if stone == 0 then countStones(1, steps - 1, cache)
				else if str.length % 2 == 0 then
					val (first, second) = str.splitAt(str.length / 2)
					countStones(first.toLong, steps - 1, cache) +
					countStones(second.toLong, steps - 1, cache)	
				else countStones(stone * 2024, steps - 1, cache)

			cache(steps).update(stone, r)
			r

val steps = 75
val cache: Vector[Map[Long, Long]] = (0 to steps).map(_ => Map.empty[Long, Long]).toVector
stones.foreach(stone => countStones(stone, steps, cache))
cache(steps).values.sum