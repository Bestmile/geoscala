package geometric

import scala.math._

case class Angle(radians: Double) extends AnyVal {
  @inline def apply(f: Double => Double): Angle = Angle(f(radians))

  @inline final def cos: Double = math.cos(radians)
  @inline final def sin: Double = math.sin(radians)
  @inline final def tan: Double = math.tan(radians)
  @inline def toDegrees: Double = math.toDegrees(radians)

  def normalize: Angle = Angle(radians % (2 * Pi))
  def invert: Angle = Angle(-radians)

  def -(angle: Angle) = apply(_ - angle.radians)
  def +(angle: Angle) = apply(_ + angle.radians)
  def <(angle: Angle) = radians < angle.radians
  def >(angle: Angle) = radians > angle.radians
}

object Angle {
  import Point2D._

  val North: Angle = Angle(0)
  val East: Angle = Angle(Pi * 0.5)
  val South: Angle = Angle(Pi)
  val West: Angle = Angle(Pi * 1.5)

  def approx(precision: Angle)(a1: Angle, a2: Angle): Boolean = abs(a1.radians - a2.radians) <= precision.radians
  def fromDegrees(degrees: Double): Angle = Angle(toRadians(degrees))
  def between[A: Point2D](p1: A, p2: A): Angle = Angle(atan2(p2.y - p1.y, p2.x - p1.x))
  // Convert an Azimuth (east of north) value to a Bearing (north of east)
  def toBearing(angle: Angle): Angle = Angle((Pi / 2) - (-angle.radians))
}
