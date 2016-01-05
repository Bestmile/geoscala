package geometric

import java.awt.geom.QuadCurve2D

import Point2D._

object JShape {
  def quadCurve2D[A: Point2D](p1: A, ctrl: A, p2: A): QuadCurve2D.Double =
    new QuadCurve2D.Double(p1.x, p1.y, ctrl.x, ctrl.y, p2.x, p2.y)
}
