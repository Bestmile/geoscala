package geometric

case class Distance(meters: Double) extends AnyVal {
  @inline def apply(f: Double => Double): Distance = Distance(f(meters))
  def -(d: Distance): Distance = apply(_ - d.meters)
  def +(d: Distance): Distance = apply(_ + d.meters)
  def <(d:Distance): Boolean = meters < d.meters
  def >(d:Distance): Boolean = meters > d.meters
}

object Distance {
  val Zero = Distance(0)
  implicit val ordering: Ordering[Distance] = Ordering.by[Distance, Double](_.meters)
}
