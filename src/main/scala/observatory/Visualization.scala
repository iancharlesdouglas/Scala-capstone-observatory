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
    val NumNeighbours = 5
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

    def findBounds(bounds: List[(Double, Color)], prevBound: (Double, Color), temp: Double): ((Double, Color), (Double, Color)) = {
      if (temp >= bounds.head._1 && !bounds.tail.isEmpty)
        findBounds(bounds.tail, bounds.head, temp)
      /*else if (temp >= bounds.head._1 && bounds.tail.isEmpty)
        (bounds.head, prevBound)*/
      else
        (bounds.head, prevBound)
    }

    val pointsArr = points.toArray
    val safeTemp = max(pointsArr(0)._1, min(points.last._1, value))

    val (lower, upper) = findBounds(points.toList, points.head, safeTemp)

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

    val pixels = new Array[Pixel](Width * Height)//.toSeq.par


    for (x <- 0 until Width; y <- 0 until Height) {
      //val longit =
      //val latit =
      val temp = predictTemperature(temperatures, Location(HalfHeight - y, x - HalfWidth))
      val colour = interpolateColor(colors, temp)
      pixels(y * Width + x) = Pixel(colour.red, colour.green, colour.blue, 255)
    }
    Image(Width, Height, pixels)
  }

}

