package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("Point closer to equatorial station than North Pole station should have temperature closer to that of the equatorial one") {

    val northPoleStation = (Location(90, -180), -10d)
    val equatorialStation = (Location(0, 0), 27d)
    val subTropicalPlace = Location(-10, -10)

    val subTropicalTemp = Visualization.predictTemperature(Seq(northPoleStation, equatorialStation), subTropicalPlace)

    assert(Math.abs(subTropicalTemp - northPoleStation._2) > Math.abs(subTropicalTemp - equatorialStation._2),
      "Diff. in temps between sub-tropical location and equatorial station should be less than between it and the North Pole")
  }

  test("Point halfway between two temperature readings will have half the value") {

    val stationA = (Location(10, 0), 20d)
    val stationB = (Location(-10, 0), 0d)
    val midpoint = Location(0, 0)

    val midpointTemp = Visualization.predictTemperature(Seq(stationA, stationB), midpoint)

    assert(midpointTemp == 10d)
  }

  test("Temperature of 50 degrees will have 255 for red and a proportionate amount of green and blue") {

    val colorScale = Seq((32d, Color(255, 0, 0)), (60d, Color(255, 255, 255)))
    val interpolatedColor = Visualization.interpolateColor(colorScale, 50d)

    assert(interpolatedColor.green == Math.round(((50d - 32d) / (60d - 32d)) * 255).toInt)
  }

  test("Temperature of 20 degrees will have a proportionate amount of green but red will be 255 and blue will be 0") {

    val colorScale = Seq((12d, Color(255, 255, 0)), (32d, Color(255, 0, 0)))
    val interpolatedColor = Visualization.interpolateColor(colorScale, 20d)

    assert(Color(255, 153, 0) == interpolatedColor)
  }

  test("Temperature of 1 degree will have red of 234, green of 255 and blue of 21") {

    val colorScale = Seq((0d, Color(0, 255, 255)), (12d, Color(255, 255, 0)))
    val interpolatedColor = Visualization.interpolateColor(colorScale, 1d)

    assert(Color(21, 255, 234) == interpolatedColor)
  }

  test("Temperature of -0.5 between -1 and 0 produces expected colour") {

    val colorScale = Seq((-1.0d, Color(255, 0, 0)), (0.0d, Color(0, 0, 255)))
    val interpolatedColor = Visualization.interpolateColor(colorScale, -0.5d)

    assert(Color(128, 0, 128) == interpolatedColor)
  }

  test("Temperature in excess of highest value on scale should return colour of that value") {

    val colorScale = Seq((32d, Color(255, 0, 0)), (60d, Color(255, 255, 255)))
    val interpolatedColor = Visualization.interpolateColor(colorScale, 70d)

    assert(Color(255, 255, 255) == interpolatedColor)
  }

  test("Temperature less than lowest value on scale should return colour of that value") {

    val colorScale = Seq((32d, Color(255, 0, 0)), (60d, Color(255, 255, 255)))
    val interpolatedColor = Visualization.interpolateColor(colorScale, 10d)

    assert(Color(255, 0, 0) == interpolatedColor)
  }

  test("Temperature closer to lower bound of lower of two bands should return colour closer to bottom-most colour than the top-most one") {

    // 12  - 255, 255, 0
    // 0   - 0, 255, 255
    // -15 - 0, 0, 255
    val colorScale = Seq((-15d, Color(0, 0, 255)), (0d, Color(0, 255, 255)), (12d, Color(255, 255, 0)))
    val interpolatedColor = Visualization.interpolateColor(colorScale, -12d)

    assert(interpolatedColor.red < 100 && interpolatedColor.green < 100 && interpolatedColor.blue > 128)
  }

  test("Image with 12 stations is returned as expected") {

    val stations = Seq((Location(10, -10), 23d), (Location(-10, 45), 24d), (Location(2, -50), 28d), (Location(8, -120), 26d),
      (Location(25, 45), 18d), (Location(30, 95), 16d), (Location(35, 135), 14d), (Location(40, 15), 12d),
      (Location(45, 105), 10d), (Location(-60, 115), 5d),
      (Location(-80, 133), -2d), (Location(85, 149), -5d))

    val colorScale = Seq((-15d, Color(0, 0, 255)), (0d, Color(0, 255, 255)), (12d, Color(255, 255, 0)), (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    val image = Visualization.visualize(stations, colorScale)

    assert(image.width == 360 && image.height == 180)
  }
  // ERROR 2
  /*
  [Test Description] [#2 - Raw data display] visualize
  [Observed Error] ArrayIndexOutOfBoundsException was thrown during property evaluation.
  Message: 64800
  Occurred when passed generated values (
    arg0 = -53.68870474015981,
    arg1 = 8.718451005203349
  )
   */
  // ERROR 3
  /*
  [Test Description] [#2 - Raw data display] predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa
  [Observed Error] NaN did not equal 10.0 +- 1.0E-4 Incorrect predicted temperature at Location(0.0,0.0): NaN. Expected: 10.0

   */
  // ERROR 4
  /*
  Test Description] [#2 - Raw data display] exceeding the greatest value of a color scale should return the color associated with the greatest value
  [Observed Error] GeneratorDrivenPropertyCheckFailedException was thrown during property evaluation.
   (VisualizationTest.scala:35)
    Falsified after 0 successful property evaluations.
    Location: (VisualizationTest.scala:35)
    Occurred when passed generated values (
      arg0 = -1.0,
      arg1 = 0.0
    )
   */
}
