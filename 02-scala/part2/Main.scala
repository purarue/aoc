import scala.io.{Source}

object Main {
  def isValid(line: String): Boolean = {
    // meh
    val Array(pol, pw) = line.split(":")
    val Array(positions, chars) = pol.split(" ")
    val char = chars.toCharArray.head
    val indices = positions.split("-").map(_.toInt)

    return indices.filter(i => pw(i) == char).size == 1
  }

  def main(args: Array[String]): Unit = {
    println(
      Source
        .fromFile(args.head)
        .getLines()
        .toList
        .filter(isValid(_))
        .size
    )
  }
}
