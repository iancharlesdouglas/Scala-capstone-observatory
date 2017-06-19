package observatory

import com.sksamuel.scrimage.{Image, Pixel}

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

    val pixels = Math.pow(2, zoom)

    val safeX = Math.min(Math.max(0, x), pixels)
    val safeY = Math.min(Math.max(0, y), pixels)

    Location(lat = 180d / Math.PI * Math.atan(Math.sinh(Math.PI - (2d * Math.PI * safeY) / pixels)),
      lon = safeX / pixels * 360d - 180)
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

    val tilePixelX = x * 256
    val tilePixelY = y * 256
    val pixels = new Array[Pixel](256 * 256)

    for (pixelY <- 0 to 255; pixelX <- 0 to 255) {
      val pixelLocation = tileLocation(8, tilePixelX + pixelX, tilePixelY + pixelY)
      val temperature = Visualization.predictTemperature(temperatures, pixelLocation)
      val color = Visualization.interpolateColor(colors, temperature)
      pixels(pixelY * 256 + pixelX) = Pixel(color.red, color.green, color.blue, 128)
    }
    Image(256, 256, pixels)
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
    ???
  }

}
