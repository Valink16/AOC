// fs contains -1 for empty spaces,
import scala.compiletime.ops.double
import scala.collection.IndexedSeqView
// and a >= 0 ID for non-empty blocks
def compact1(fs: Array[Int]) =
	val rev = fs.view.reverse
	var emptyPointer = fs.indexWhere(_ == -1)
	var revBlockPointer = rev.indexWhere(_ != -1)
	while emptyPointer != -1 &&
		  revBlockPointer != -1 &&
		  emptyPointer < fs.length - 1 - revBlockPointer do
		fs.update(emptyPointer, fs(fs.length - 1 - revBlockPointer))
		fs.update(fs.length - 1 - revBlockPointer, -1)
		emptyPointer = fs.indexWhere(_ == -1, from = emptyPointer + 1)
		revBlockPointer = rev.indexWhere(_ != -1, from = revBlockPointer + 1)
	fs

def invIndex(length: Int, index: Int) = length - 1 - index

def findBlock(view: IndexedSeqView[Int], from: Int, pred: Int => Boolean): Option[(Int, Int)] =
	val blockStart = view.indexWhere(pred, from)
	if blockStart == -1 then
		None
	else
		val id = view(blockStart)
		val blockEnd = view.indexWhere(_ != id, from = blockStart)
		if blockEnd == -1 then
			Some(blockStart, view.length - blockStart)
		else
			val blockSize = blockEnd - blockStart
			Some(blockStart, blockSize)

def compact2(fs: Array[Int]): Array[Int] =
	val view = fs.view
	val rev = view.reverse
	
	var block = findBlock(rev, 0, _ != -1)
	while true do
		block match
			case Some(b) =>
				var empty = findBlock(view, 0, _ == -1)
				while empty.isDefined do
					empty = empty match
						case Some(e) =>
							if e._1 < invIndex(fs.length, b._1) &&
							   e._2 >= b._2 then
								(0 until b._2).foreach(i =>
									fs.update(e._1 + i, fs(invIndex(fs.length, b._1 + i)))	
									fs.update(invIndex(fs.length, b._1 + i), -1)
								)
								None
							else
								findBlock(view, e._1 + e._2, _ == -1)
						case None => None

				block = findBlock(rev, b._1 + b._2, _ != -1)
			case None => return fs
	fs

scala.io.Source.fromFile("inputs/day9.txt").getLines().map(line =>
	line.zipWithIndex.flatMap((c, i) =>
		if i % 2 == 0 then
			(0 until c.toInt - '0').map(_ => i / 2)
		else	
			(0 until c.toInt - '0').map(_ => -1)
	)
).map(s =>
	compact2(s.toArray)
		.zipWithIndex
		.filter(_._1 != -1)
		.foldLeft(0L)((acc, indexedBlock) =>
			acc + indexedBlock._1 * indexedBlock._2
		)
).toList
