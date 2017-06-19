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

  test("Bounds checks") {
    check((x: Int, y: Int) => {
      val location = Interaction.tileLocation(3, x, y)
      location.lon >= -180 && location.lon <= 180 && location.lat >= -90 && location.lat <= 90
    }, maxSize(20), minSuccessful(10))
  }
}
