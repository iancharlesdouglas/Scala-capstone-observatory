package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import java.lang.Math._

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

    def distanceBetweenGreatCircleMethod(loc1 : Location, loc2 : Location) =
      acos(sin(toRadians(loc1.lat)) * sin(toRadians(loc2.lat)) +
        cos(toRadians(loc1.lat)) * cos(toRadians(loc2.lat)) * cos(toRadians(loc2.lon) - toRadians(loc1.lon))) * RadiusOfEarth

    //def square(value : Double) = value * value

    def findClosest(neighbours : Int) : Iterable[(Location, Double)] =
      temperatures.map(t => (t, abs(distanceBetweenGreatCircleMethod(t._1, location))))     //square(location.lat - t._1.lat) + square(location.lon - t._1.lon)))
        .toSeq.sortBy(_._2).reverse
        .take(neighbours)
        .map(_._1)

    def inverseDistanceWeighting(loc1: Location, loc2: Location, power: Double = 2d): Double =
      pow(1d / distanceBetweenGreatCircleMethod(loc1, loc2), power)

    val closest = findClosest(NumNeighbours)
    closest.map(c => inverseDistanceWeighting(c._1, location) * c._2).sum / NumNeighbours
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
    ???
  }

}

