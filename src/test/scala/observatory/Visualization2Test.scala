package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.math.BigDecimal.RoundingMode

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite with Checkers {

  implicit class DoubleExtensions(value: Double) {
    def roundTo(places: Int): Double = BigDecimal(value).setScale(places, RoundingMode.HALF_UP).toDouble
  }

  test("Bilinear interpolation produces expected results") {

    assert(Visualization2.bilinearInterpolation(0.5, 0.5, 10.0, 10.0, 20.0, 20.0).roundTo(4) == 15.0)

    assert(Visualization2.bilinearInterpolation(1.0 / 3, 1.0 / 3, 10.0, 10.0, 20.0, 20.0).roundTo(4) == (10 + 10.0 / 3).roundTo(4))
  }
}
