package geodesy
// packages: jts, geojson, pgslick.

/** A coordinate reference system  **/
case class CRS(srid: Int)

/** A georeference value with her CRS **/
case class Ref[A](crs: CRS, value: A)
object Ref {
  trait Companion[G] {
    object Ref { def apply(crs: CRS, g: G): Ref = geodesy.Ref(crs, g) }
    type Ref = geodesy.Ref[G]
  }
}
