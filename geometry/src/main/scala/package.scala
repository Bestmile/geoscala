import scala.language.higherKinds

import geometric._
import Point2D._

package object geometric {
  type JLine2D = java.awt.geom.Line2D
  type JPoint2D = java.awt.geom.Point2D
  type JShape = java.awt.Shape

  object JLine2D {
    def apply[A: Point2D](p1: A, p2: A): JLine2D =
      new java.awt.geom.Line2D.Double(p1.x, p1.y, p2.x, p2.y)
  }

  object JPoint2D {
    def apply(x: Double, y: Double): JPoint2D = new java.awt.geom.Point2D.Double(x, y)
  }

  implicit class RichJShape(self: JShape) {
    def pathOf[A: Point2D](flatness: Double): List[A] = {
      val iterator = self.getPathIterator(null, flatness)

      val buffer = Array.ofDim[Double](2)
      val results = new collection.mutable.ArrayBuffer[A]()

      while (!iterator.isDone()) {
        iterator.currentSegment(buffer)
        results += Point2D[A](buffer(0), buffer(1))
        iterator.next()
      }

      results.toList
    }
  }

  implicit val tuplePoint2D: Point2D[(Double, Double)] = new Point2D[(Double, Double)] {
    def apply(x: Double, y: Double) = (x, y)
    def x(self: (Double, Double)): Double = self._1
    def y(self: (Double, Double)): Double = self._2
  }

  implicit val jpoint2DPoint2D: Point2D[JPoint2D] = new Point2D[JPoint2D] {
    def apply(x: Double, y: Double): JPoint2D = JPoint2D(x, y)
    def x(coord: JPoint2D): Double = coord.getX
    def y(coord: JPoint2D): Double = coord.getY
  }
}

