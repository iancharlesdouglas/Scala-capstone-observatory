package observatory

import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{Image, Pixel}
import java.lang.Math._

import scala.collection
import scala.collection.parallel.mutable.ParArray

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val RadiusOfEarth = 6371d

  def distanceBetweenGreatCircleMethod(loc1 : Location, loc2 : Location) = {
    acos(sin(toRadians(loc1.lat)) * sin(toRadians(loc2.lat)) +
      cos(toRadians(loc1.lat)) * cos(toRadians(loc2.lat)) * cos(toRadians(loc2.lon) - toRadians(loc1.lon))) * RadiusOfEarth
  }

  def distanceBetweenVincentyFormula(loc1: Location, loc2: Location) = {
    val (lat1, lat2, lon1, lon2) = (toRadians(loc1.lat), toRadians(loc2.lat), toRadians(loc1.lon), toRadians(loc2.lon))
    val diffLon = lon2 - lon1

    val distance = atan2(
      sqrt(pow(cos(lat2) * sin(diffLon), 2) + pow(cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(diffLon), 2)),
      sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(diffLon)
    ) * RadiusOfEarth * 1000

    if (distance == Double.NaN)
      1d
    else
      distance
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    val NumNeighbours = 5
    val Power = 2d
    /*val FocusRadius = 5

    if (temperatures.size == 0) return 0d

    val (focusLonMin, focusLonMax, focusLatMin, focusLatMax) = (location.lon - FocusRadius, location.lon + FocusRadius,
      location.lat - FocusRadius, location.lat + FocusRadius)
    var stations = temperatures.filter(s => s._1.lon >= focusLonMin && s._1.lon <= focusLonMax &&
      s._1.lat >= focusLatMin && s._1.lat <= focusLatMax)
    if (stations.size < NumNeighbours) stations = temperatures*/

    /*def findClosest(neighbours : Int) : Iterable[((Location, Double), Double)] =
      temperatures/*stations*/.map(t => (t, abs(distanceBetweenVicentyFormula(t._1, location))))
        .toSeq.sortBy(_._2)
        .take(neighbours)*/

    val closest = temperatures.map(t => (t, abs(distanceBetweenVincentyFormula(t._1, location))))
    .toSeq.sortBy(_._2)
    .take(NumNeighbours)

    //val closest = findClosest(NumNeighbours)//.map(c => (c, abs(distanceBetweenGreatCircleMethod(c._1, location))))

    if (closest.size > 0 && (closest.head._1._1 == location || closest.head._2 == 0d))
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

    def round(value: Double, decimalPlaces: Int) =
      BigDecimal(value).setScale(decimalPlaces, BigDecimal.RoundingMode.HALF_UP).toDouble

    Color(round(portion * (upper._2.red - lower._2.red) + lower._2.red, 0).toInt,
      round(portion * (upper._2.green - lower._2.green) + lower._2.green, 0).toInt,
      round(portion * (upper._2.blue - lower._2.blue) + lower._2.blue, 0).toInt)
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

    /*def pixelFrom(startX: Int, startY: Int, pixels: List[Pixel]): List[Pixel] = {
      if (startX == Width && startY == Height)
        pixels
      else if (startX == Width)
        pixelFrom(0, startY + 1, pixels)
      else {
        val temp = predictTemperature(temperatures, Location(HalfHeight - startY, startX - HalfWidth))
        val color = interpolateColor(colors, temp)
        pixelFrom(startX + 1, startY, pixels.::(Pixel(color.red, color.green, color.blue, 128)))
      }
    }

    Image(Width, Height, pixelFrom(0, 0, List()).toArray)*/

    val pixels = (0 until Width * Height).map { index =>
      val (y, x) = (index / Width, index % Width)
      val temp = predictTemperature(temperatures, Location(HalfHeight - y, x - HalfWidth))
      val color = interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, 255)
    }.toArray

    Image(Width, Height, pixels)
  }
}

