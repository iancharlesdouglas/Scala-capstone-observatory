package observatory

import java.nio.file.Paths

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("Stations file can be opened"){
    val extraction = Extraction.locateTemperatures(1995, "/stations.csv", "/1995.csv")
  }

  /** @return The filesystem path of the given resource */
  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString
}