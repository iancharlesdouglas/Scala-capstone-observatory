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

    assert(Math.abs(midpointTemp - 10d) < 0.1d)
  }

  test("Points 1 degree apart will be approx. 111km apart") {

    assert(Visualization.distanceBetweenGreatCircleMethod(Location(1, 0), Location(0, 0)).round == 111)

    assert(Visualization.distanceBetweenGreatCircleMethod(Location(0, 1), Location(0, 0)).round == 111)

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

  test("Colour interpolation works as expected") {

    val colorScale = Seq((-60d, Color(0, 0, 0)),
      (-50d, Color(33, 0, 107)),
      (-27d, Color(255, 0, 255)),
      (-15d, Color(0, 0, 255)),
      (0d, Color(0, 255, 255)),
      (12d, Color(255, 255, 0)),
      (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    var interpolatedColor = Visualization.interpolateColor(colorScale, -56.6666d)
    assert(interpolatedColor == Color(11, 0, 36))

    interpolatedColor = Visualization.interpolateColor(colorScale, -10d)
    assert(interpolatedColor == Color(0, 85, 255))

    interpolatedColor = Visualization.interpolateColor(colorScale, 39d)
    assert(interpolatedColor == Color(255, 64, 64))
  }

  test("Color interpolation 2") {

    val stations = List((Location(45.0, -90.0), 14.683824588037325),
      (Location(-45.0, 0.0), 61.69359549202767))

    val colors = List((14.683824588037325,Color(255,0,0)), (61.69359549202767,Color(0,0,255)))

    val location = Location(-28.0, -176.0)

    val temperature = Visualization.predictTemperature(stations, location)

    val distances = stations.map(s => Visualization.distanceBetweenVincentyFormula(s._1, Location(-28.0, -176.0)))

    val locationColor = Visualization.interpolateColor(colors, temperature)

    val xxx = 1
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

    //image.output(new java.io.File("c:\\users\\Ian\\projects\\scala\\capstone\\x.png"))
  }

  test("Color scale with match on a single scale entry boundary interpolates correctly") {

    val colorScale = Seq((-60d, Color(0, 0, 0)),
      (-50d, Color(33, 0, 107)),
      (-27d, Color(255, 0, 255)),
      (-15d, Color(0, 0, 255)),
      (0d, Color(0, 255, 255)),
      (12d, Color(255, 255, 0)),
      (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    val interpolatedColor = Visualization.interpolateColor(colorScale, -50d)

    assert(interpolatedColor == Color(33, 0, 107))
  }

  ignore("Extraction data rendered as image") {

    val extraction = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    val readings = Extraction.locationYearlyAverageRecords(extraction)

    val colorScale = Seq((-60d, Color(0, 0, 0)),
      (-50d, Color(33, 0, 107)),
      (-27d, Color(255, 0, 255)),
      (-15d, Color(0, 0, 255)),
      (0d, Color(0, 255, 255)),
      (12d, Color(255, 255, 0)),
      (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    val image = Visualization.visualize(readings, colorScale)

    image.output(new java.io.File("c:\\users\\Ian\\projects\\scala\\capstone\\1975.png"))
  }

  test("Single reading test") {

    val stations = Seq((Location(1.0, -2.63222101E8), 2.6631255615362604E-250))

    val colorScale = Seq((-15d, Color(0, 0, 255)), (0d, Color(0, 255, 255)), (12d, Color(255, 255, 0)), (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    val image = Visualization.visualize(stations, colorScale)

    assert(image.width == 360 && image.height == 180)
  }

  /*ignore("Generated stations test") {

    val colorScale = Seq((-15d, Color(0, 0, 255)), (0d, Color(0, 255, 255)), (12d, Color(255, 255, 0)), (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    check({
      (stationsParams: Iterable[(Int, Int, Double)]) => {

        val stations = stationsParams.map(s => (Location(s._1, s._2), s._3))

        val image = Visualization.visualize(stations, colorScale)

        image.width == 360 && image.height == 180
      }
    }, minSize(10), maxSize(20), minSuccessful(20), workers(4))
  }*/

  test("Generated colour scale and stations test") {

    check({
      (stationsParams: Iterable[(Int, Int, Double)], colorParams: Iterable[(Double, Byte, Byte, Byte)]) => {

        val stations = stationsParams.map(s => (Location(s._1, s._2), s._3))

        val colorScale = colorParams.map(c => (c._1, Color(c._2 + 128, c._3 + 128, c._4 + 128))).toList.sortBy(_._1).toIterable

        val image = Visualization.visualize(stations, colorScale)

        image.width == 360 && image.height == 180
      }
    }, minSize(8), maxSize(20), minSuccessful(80),workers(4))
  }

  test("Known temp. NaN test") {

    val stations = Seq((Location(2.12886296E8, 1.602617134E9), 7.66636799436318E18))

    val colorScale = Seq((-15d, Color(0, 0, 255)), (0d, Color(0, 255, 255)), (12d, Color(255, 255, 0)), (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    val image = Visualization.visualize(stations, colorScale)

    assert(image.width == 360 && image.height == 180)
  }
}
