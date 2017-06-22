package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("Tile(1, 1) at zoom level 1 should be at location (0, 0)") {
    val location = Interaction.tileLocation(1, 1, 1)
    assert(location == Location(0, 0))
  }

  test("Tile(4, 4) at zoom level 3 should be at location (0, 0)") {
    val location = Interaction.tileLocation(3, 4, 4)
    assert(location == Location(0, 0))
  }

  test("Tile(4, 5) at zoom level 3 should be at latitude of -22.5 or lower and longitude of 0)") {
    val location = Interaction.tileLocation(3, 4, 5)
    assert(location.lat < -22.5d && location.lon == 0)
  }

  test("Tile(5, 5) at zoom level 3 should be at longitude 45") {
    val location = Interaction.tileLocation(3, 5, 5)
    assert(location.lon == 45d)
  }

  test("Tile(3, 5) at zoom level 3 should be at longitude -45") {
    val location = Interaction.tileLocation(3, 3, 5)
    assert(location.lon == -45d)
  }

  ignore("Bounds checks") {
    check((x: Int, y: Int) => {
      val location = Interaction.tileLocation(3, x, y)
      location.lon >= -180 && location.lon <= 180 && location.lat >= -90 && location.lat <= 90
    }, maxSize(20), minSuccessful(10))
  }

  test("Pixels are consistent between different zoom levels") {

    val colorScale = Seq((-60d, Color(0, 0, 0)),
      (-50d, Color(33, 0, 107)),
      (-27d, Color(255, 0, 255)),
      (-15d, Color(0, 0, 255)),
      (0d, Color(0, 255, 255)),
      (12d, Color(255, 255, 0)),
      (32d, Color(255, 0, 0)),
      (60d, Color(255, 255, 255)))

    val stations = Seq((Location(90.0, -180.0), -30.0), (Location(-90.0, 180.0), -30.0), (Location(0.0, 0.0), 30.0))

    val mainImg = Interaction.tile(stations, colorScale, 0, 0, 0)

    (0 to 3).foreach { tile =>

      val (tileX, tileY) = (tile % 2, tile / 2)
      val subImg = Interaction.tile(stations, colorScale, 1, tileX, tileY)

      (0 until 128 * 128).foreach { index =>
        val (mainX, mainY) = (index % 128 + tileX * 128, index / 128 + tileY * 128)
        val mainPixel = mainImg.pixel(mainX, mainY).toColor
        var subPixel = subImg.pixel((index % 128) * 2, (index / 128) * 2).toColor
        assert(mainPixel == subPixel)
      }
    }
    /*(0 to 3).foreach { index =>
      val (x, y) = (index % 2, index / 2)
      val img1 = Interaction.tile(stations, colorScale, 1, x, y)
      val pixel1 = img1.pixel(128, 0)
      val color1 = pixel1.toColor
      img1.output(new java.io.File(s"c:\\users\\Ian\\projects\\scala\\capstone\\zoom-1-$index.png"))
    }*/
  }
}
