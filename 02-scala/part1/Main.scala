import scala.io.{Source}

object Main {
  def isValid(line: String): Boolean = {
    // meh
    val Array(pol, pw) = line.split(":")
    val Array(srange, chars) = pol.split(" ")
    val Array(min, max) = srange.split("-").map(_.toInt)

    val occurences = pw.count(_ == chars.toCharArray.head)

    return min <= occurences && occurences <= max
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
