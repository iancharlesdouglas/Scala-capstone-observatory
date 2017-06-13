package observatory

import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{Image, Pixel}
import java.lang.Math._
import scala.collection

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
    val NumNeighbours = 2
    val Power = 2d

    def distanceBetweenGreatCircleMethod(loc1 : Location, loc2 : Location) =
      acos(sin(toRadians(loc1.lat)) * sin(toRadians(loc2.lat)) +
        cos(toRadians(loc1.lat)) * cos(toRadians(loc2.lat)) * cos(toRadians(loc2.lon) - toRadians(loc1.lon))) * RadiusOfEarth

    def findClosest(neighbours : Int) : Iterable[(Location, Double)] =
      temperatures.map(t => (t, abs(distanceBetweenGreatCircleMethod(t._1, location))))
        .toSeq.sortBy(_._2)
        .take(neighbours)
        .map(_._1)

    val closest = findClosest(NumNeighbours).map(c => (c, abs(distanceBetweenGreatCircleMethod(c._1, location))))

    closest.map(c => c._1._2 / pow(c._2, Power)).sum / closest.map(c => 1d / pow(c._2, Power)).sum
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    def findBounds(bounds: List[(Double, Color)], prevBound: (Double, Color)): ((Double, Color), (Double, Color)) = {
      if (value < bounds.head._1)
        findBounds(bounds.tail, bounds.head)
      else if (value == bounds.head._1)
        (bounds.head, bounds.head)
      else
        (bounds.head, prevBound)
    }

    val pointsArr = points.toArray
    val safeTemp = max(pointsArr(0)._1, min(points.last._1, value))

    val (lower, upper) = findBounds(points.toList, points.head)

    val portion = (value - lower._1) / (upper._1 - lower._1)

    Color((portion * (upper._2.red - lower._2.red) + lower._2.red).toInt,
      (portion * (upper._2.green - lower._2.green) + lower._2.green).toInt,
      (portion * (upper._2.blue - lower._2.blue) + lower._2.blue).toInt)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val pixels = new Array[Pixel](360 * 180)

    for (x <- 0 until 360; y <- 0 until 180) {
      val longit = x - 180
      val latit = 90 - y
      val temp = predictTemperature(temperatures, Location(latit, longit))
      val colour = interpolateColor(colors, temp)
      pixels(x * 360 + y) = Pixel(colour.red, colour.green, colour.blue, 255)
    }
    Image(360, 180, pixels)
  }

}

