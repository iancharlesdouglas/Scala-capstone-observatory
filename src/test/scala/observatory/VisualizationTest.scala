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

  test("Temperature of 20 degrees will have some green but red will be 255 and blue will be 0") {

    val colorScale = Seq((32d, Color(255, 0, 0)), (12d, Color(255, 255, 0)))
    val interpolatedColor = Visualization.interpolateColor(colorScale, 20d)

    assert(Color(255, 153, 0) == interpolatedColor)
  }

  test("Temperature of 1 degree will have red of 234, green of 255 and blue of 21") {

    val colorScale = Seq((12d, Color(255, 255, 0)), (0d, Color(0, 255, 255)))
    val interpolatedColor = Visualization.interpolateColor(colorScale, 1d)

    assert(Color(21, 255, 233) == interpolatedColor)
  }
}
