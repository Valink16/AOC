import scala.util.matching.Regex

val mulRe = new Regex("""(do\(\)|don't\(\))|mul\(([0-9]{1,3}),([0-9]{1,3})\)""", "do", "lhs", "rhs")

val s = scala.io.Source.fromFile("inputs/day3.txt").mkString

val firstMatch = mulRe.findAllMatchIn(s).next()

firstMatch.group("do") match
  case null => false
  case s => s == "do()"

val matches = mulRe.findAllMatchIn(s).foldLeft((0, true))((acc, m) =>
  m.group("do") match
      case null => // This means it's the mul group which got matched
        if acc._2 then
          acc.copy(_1 = acc._1 + m.group("lhs").toInt * m.group("rhs").toInt)
        else
          acc
      case s =>
        acc.copy(_2 = s == "do()")
)
