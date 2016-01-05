package geometric

import scala.annotation.tailrec

import math._

trait Point2D[A] {
  def apply(x: Double, y: Double): A
  def x(a: A): Double
  def y(a: A): Double
}

object Point2D {
  def apply[A](x: Double, y: Double)(implicit A: Point2D[A]): A = A(x, y)

  def distance[A: Point2D](p1: A, p2: A): Distance = {
    val dx = p1.x - p2.x
    val dy = p1.y - p2.y
    Distance(sqrt(dx*dx + dy*dy))
  }

  def distancePath[A: Point2D](path: List[A]): Distance = {
    @tailrec
    def f(p0: A, ps0: List[A], d: Distance): Distance =
      ps0 match {
        case p1::ps1 =>
          f(p1, ps1, Point2D.distance[A](p0, p1) + d)
        case Nil => d
      }

    path match {
      case p::ps =>
        f(p, ps, Distance.Zero)
      case _ =>
        Distance.Zero
    }
  }

  def move[A: Point2D](distance: Distance, angle: Angle): A => A =
    p => Point2D[A](p.x + (angle.cos * distance.meters), p.y + (angle.sin * distance.meters))

  def sortClockwise[A: Point2D](origin: A, ps: List[A]): List[A] = {
    ps.sortWith { (a, b) =>
      val comparing =
        (a.x - origin.x) * (b.y - origin.y) - (b.x - origin.x) * (a.y - origin.y)
      comparing < 0
    }
  }

  implicit class Point2DOps[A](a: A)(implicit A: Point2D[A]) {
    @inline final def x = A.x(a)
    @inline final def y = A.y(a)
    def asPoint2D[B: Point2D]: B = Point2D[B](a.x, a.y)
    @inline final def distance(p2: A) = Point2D.distance(a, p2)

    @inline final def move(distance: Distance, angle: Angle): A =
      Point2D.move[A](distance, angle).apply(a)

    @inline final def moveTo(p2: A, distance: Distance): A =
      Point2D.move[A](distance, Angle.between(a, p2)).apply(a)
  }
}
