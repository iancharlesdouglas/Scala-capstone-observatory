package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /** Returns the latitude and longitude of the top-left corner of the tile with the X and Y coordinates, per the zoom level.
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {

    val tiles = pow(2d, zoom)

    Location(lat = atan(sinh(PI * (1.0 - 2.0 * y / tiles))) * 180.0 / PI, lon = x / tiles * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {

    val Size = 256
    //System.out.println(s"Tile - zoom: $zoom; x: $x; y: $y")

    val pixels = (0 until Size * Size).toArray.par.map { index =>
      val (pixelY, pixelX) = (index / Size, index % Size)
      val pixelLocation = tileLocation(zoom + 8, pixelX + x * Size, pixelY + y * Size)
      val temperature = Visualization.predictTemperature(temperatures, Location(pixelLocation.lat, pixelLocation.lon))
      val color = Visualization.interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, 128)
    }.toArray
    Image(Size, Size, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    val bin = for {
      (year, readings) <- yearlyData
      zoom  <- 0 to 3
      x <- 0 until Math.pow(2, zoom).toInt
      y <- 0 until Math.pow(2, zoom).toInt
    } {
      generateImage(year, zoom, x, y, readings)
    }
  }

}
