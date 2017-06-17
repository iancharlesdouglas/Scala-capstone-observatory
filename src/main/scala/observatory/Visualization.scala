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

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    val RadiusOfEarth = 6371d
    val NumNeighbours = 3
    val Power = 2d
    val FocusRadius = 5

    if (temperatures.size == 0) {
      return 0d
    }

    def distanceBetweenGreatCircleMethod(loc1 : Location, loc2 : Location) =
      acos(sin(toRadians(loc1.lat)) * sin(toRadians(loc2.lat)) +
        cos(toRadians(loc1.lat)) * cos(toRadians(loc2.lat)) * cos(toRadians(loc2.lon) - toRadians(loc1.lon))) * RadiusOfEarth

    val (focusLonMin, focusLonMax, focusLatMin, focusLatMax) = (location.lon - FocusRadius, location.lon + FocusRadius,
      location.lat - FocusRadius, location.lat + FocusRadius)
    var stations = temperatures.filter(s => s._1.lon >= focusLonMin && s._1.lon <= focusLonMax &&
      s._1.lat >= focusLatMin && s._1.lat <= focusLatMax)
    if (stations.size < NumNeighbours) stations = temperatures

    def findClosest(neighbours : Int) : Iterable[(Location, Double)] =
      stations.map(t => (t, abs(distanceBetweenGreatCircleMethod(t._1, location))))
        .toSeq.sortBy(_._2)
        .take(neighbours)
        .map(_._1)

    val closest = findClosest(NumNeighbours).map(c => (c, abs(distanceBetweenGreatCircleMethod(c._1, location))))

    if (!closest.isEmpty && (closest.head._1._1 == location || closest.head._2 == 0d))
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

    val colorScaleList = points.toList
    val colorScale = colorScaleList.zip(colorScaleList.tail)

    if (colorScaleList.isEmpty) Color(0, 0, 0)

    val safeTemp = max(colorScaleList.head._1, min(points.last._1, value))

    val (lower, upper) = colorScale.filter(s => safeTemp >= s._1._1 && safeTemp <= s._2._1).head

    val portion = (safeTemp - lower._1) / (upper._1 - lower._1)

    Color(Math.round(portion * (upper._2.red - lower._2.red) + lower._2.red).toInt,
      Math.round(portion * (upper._2.green - lower._2.green) + lower._2.green).toInt,
      Math.round(portion * (upper._2.blue - lower._2.blue) + lower._2.blue).toInt)
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

    val coords = new Array[(Int, Int)](Width * Height)

    for (x <- 0 until Width; y <- 0 until Height)
      coords(y * Width + x) = (y, x)

    val pixels = coords.map { coord =>
      val (y, x) = coord
      val temp = predictTemperature(temperatures, Location(HalfHeight - y, x - HalfWidth))
      val color = interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, 255)
    }

    Image(Width, Height, pixels)
  }
}

