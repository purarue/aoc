import scala.io.{Source}
import collection.mutable.{Map}

abstract class WireSegment
case class Up(dist: Integer) extends WireSegment
case class Left(dist: Integer) extends WireSegment
case class Right(dist: Integer) extends WireSegment
case class Down(dist: Integer) extends WireSegment

object Segment {
  def apply(str: String): Option[WireSegment] = str match {
    case u if u.startsWith("U") => Some(Up(u.stripPrefix("U").toInt))
    case l if l.startsWith("L") => Some(Left(l.stripPrefix("L").toInt))
    case r if r.startsWith("R") => Some(Right(r.stripPrefix("R").toInt))
    case d if d.startsWith("D") => Some(Down(d.stripPrefix("D").toInt))
    case _                      => None
  }
}

class Position(var x: Int = 0, var y: Int = 0)
class Direction(val x: Boolean, val y: Boolean)

class Trace(val segments: List[WireSegment]) {

  // current position
  var pos: Position = new Position()

  override def toString(): String = {
    val segmentStr = segments
      .map({ w: WireSegment => String.valueOf(w) })
      .toList
      .mkString(", ")
    return s"Trace($segmentStr)"
  }

  // run wire segment at index and return a list of X, Y locations this touched
  def runSegment(index: Int): List[(Int, Int)] {
    segments[index]
  }
}

class Grid(val traces: List[Trace]) {

  private val grid = Map[(Int, Int), Int]()

  def runTrace() {
    for (trace <- traces) {
      for (segment <- trace.segments) {
        println(segment)
      }
    }
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    val wires: List[Trace] =
      Source
        .fromFile(args.head)
        .getLines
        .toList
        .map({ line: String =>
          line.trim
            .split(",")
            .map({ segmentStr => Segment(segmentStr).get })
            .toList
        })
        .map({ segmentList => new Trace(segmentList) })

    val g = new Grid(wires)
    g.runTrace

  }
}
