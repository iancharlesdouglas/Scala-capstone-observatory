package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import java.lang.Math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val RadiusOfEarth = 6371.0

  def distanceBetweenGreatCircleMethod(loc1 : Location, loc2 : Location) = {

    val (lat1, lat2) = (toRadians(loc1.lat), toRadians(loc2.lat))

    val distance = acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(toRadians(loc2.lon) - toRadians(loc1.lon))) * RadiusOfEarth

    if (distance == Double.NaN) 1.0 else distance
  }

  // The Vincenty formula is compute-intensive and slow
  def distanceBetweenVincentyFormula(loc1: Location, loc2: Location) = {
    val (lat1, lat2, lon1, lon2) = (toRadians(loc1.lat), toRadians(loc2.lat), toRadians(loc1.lon), toRadians(loc2.lon))
    val diffLon = lon2 - lon1

    val distance = atan2(
      sqrt(pow(cos(lat2) * sin(diffLon), 2) + pow(cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(diffLon), 2)),
      sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(diffLon)
    ) * RadiusOfEarth

    if (distance == Double.NaN) 1.0 else distance
  }

  //lazy val
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    val NumNeighbours = 9 //3
    val Power = 2.0

    val closest = temperatures.map(t => (t, abs(distanceBetweenGreatCircleMethod(t._1, location))))
    .toArray.sortBy(_._2)
    .take(NumNeighbours)

    if (closest.size > 0 && (closest.head._1._1 == location || closest.head._2 <= 1d))
      closest.head._1._2
    else
      closest.map(c => c._1._2 / pow(c._2, Power)).sum / closest.map(c => 1d / pow(c._2, Power)).sum
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    if (points.size == 0) return Color(0, 0, 0)
    else if (points.size == 1) return points.head._2

    val scale = if (points.head._1 < points.last._1) points.toArray else points.toArray.sortBy(_._1)

    val scaleIndex = scale.indexWhere(value <= _._1)

    val safeTemp =
      if (scaleIndex == -1)
        scale.last._1
      else if (scaleIndex == 0)
        scale(0)._1
      else
        value

    val upperIndex = if (scaleIndex == -1) scale.length - 1 else if (scaleIndex == 0) 1 else scaleIndex
    val lowerIndex = if (upperIndex > 0) upperIndex - 1 else upperIndex

    val (lower, upper) = (scale(lowerIndex), scale(upperIndex))

    val portion = (safeTemp - lower._1) / (upper._1 - lower._1)

    Color(round(portion * (upper._2.red - lower._2.red) + lower._2.red).toInt,
      round(portion * (upper._2.green - lower._2.green) + lower._2.green).toInt,
      round(portion * (upper._2.blue - lower._2.blue) + lower._2.blue).toInt)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {

    val Width = 360
    val Height = 180
    val HalfWidth = Width / 2
    val HalfHeight = Height / 2

    val pixels = (0 until Width * Height).toArray.par.map { index =>
      val (y, x) = (index / Width, index % Width)
      val temp = predictTemperature(temperatures, Location(HalfHeight - y, x - HalfWidth))
      val color = interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, 255)
    }.toArray

    Image(Width, Height, pixels)
  }
}

