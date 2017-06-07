package observatory

import com.sksamuel.scrimage.{Image, Pixel}

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

    def square(value : Double) = value * value

    def findClosest(neighbours : Int) : Iterable[(Location, Double)] =
      temperatures.map(t => (t, square(location.lat - t._1.lat) + square(location.lon - t._1.lon)))
        .toSeq.sortBy(_._2).reverse
        .take(neighbours)
        .map(_._1)

    def inverseDistanceWeighting(location1 : Location, location2 : Location, power : Double = 2d) : Double =
      Math.pow(1d / distanceBetween(location1, location2), power)

    def distanceBetween(location1 : Location, location2 : Location) = ???
    
    ???
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    ???
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

