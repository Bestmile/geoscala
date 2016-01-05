package geodesy

sealed trait Geometry extends Any

object Geometry {
  sealed trait Core extends Geometry

  def intersects(g0: Geometry, g1: Geometry): Boolean = ???

  def points(g: Geometry): List[Point] =
    foldSum[List[Point]](
      point = List(_),
      line = identity,
      polygon = p => p.shell.points ++ p.holes.flatMap(_.points),
      sum = _.flatten
    )(g)

  def foldSum[A](
    point: Point => A,
    line: List[Point] => A,
    polygon: Polygon => A,
    sum: List[A] => A
  )(g: Geometry): A = {
    def flatten(xs: List[Geometry]) = sum(xs.map(foldSum(point, line, polygon, sum)))
    g match {
      case p: Point => point(p)
      case p: Polygon => polygon(p)
      case l: LinearRing => line(l.points)
      case LineString(xs) => line(xs)
      case MultiPoint(xs) => flatten(xs)
      case MultiLineString(xs) => flatten(xs)
      case MultiPolygon(xs) => flatten(xs)
    }
  }
}

class Point(x: Double, y: Double, z: Double) extends Geometry
object Point extends Ref.Companion[Point] {
  def apply(x: Double, y: Double): Point = new Point(x, y, Double.NaN)
  def apply3(x: Double, y: Double, z: Double): Point = new Point(x, y, z)
}

case class MultiPoint(points: List[Point]) extends AnyVal with Geometry
object MultPoint extends Ref.Companion[Point]

case class LineString(points: List[Point]) extends AnyVal with Geometry
object LineString extends Ref.Companion[LineString]

case class MultiLineString(lines: List[LineString]) extends AnyVal with Geometry
object MultiLineString extends Ref.Companion[MultiLineString]

class LinearRing private[geodesy] (val points: List[Point]) extends AnyVal with Geometry
object LinearRing extends Ref.Companion[LinearRing] {
  def apply(points: List[Point]): Option[LinearRing] = ???
  def unsafe(points: List[Point]): LinearRing = new LinearRing(points)
}

case class Polygon(shell: LinearRing, holes: List[LinearRing]) extends Geometry
object Polygon extends Ref.Companion[Polygon]

case class MultiPolygon(polygons: List[LineString]) extends AnyVal with Geometry
object MultiPolygon extends Ref.Companion[MultiPolygon]
