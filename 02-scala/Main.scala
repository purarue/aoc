import scala.io.{Source}

object Main {
  def validPartOne(line: String): Boolean = {
    val Array(pol, pw) = line.split(":")
    val Array(srange, chars) = pol.split(" ")
    val Array(min, max) = srange.split("-").map(_.toInt)
    val occurrences = pw.count(_ == chars.toCharArray.head)
    return min <= occurrences && occurrences <= max
  }

  def validPartTwo(line: String): Boolean = {
    val Array(pol, pw) = line.split(":")
    val Array(positions, chars) = pol.split(" ")
    val indices = positions.split("-").map(_.toInt)
    return indices.filter(i => pw(i) == chars(0)).size == 1
  }

  def main(args: Array[String]): Unit = {
    val lines = Source
      .fromFile(args.head)
      .getLines()
      .toList
    println("Part 1: %d" format (lines.filter(validPartOne(_)).size))
    println("Part 2: %d" format (lines.filter(validPartTwo(_)).size))
  }
}
