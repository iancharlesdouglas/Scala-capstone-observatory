package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class WeatherStation(stnIdentifier: String, wbanIdentifier: String, latitude: Double, longitude: Double)  //, uniqueIdentifier: String)
//case class WeatherStation(stnIdentifier: String, wbanIdentifier: Option[String], latitude: Option[Double], longitude: Option[Double], uniqueIdentifier: Option[String])

case class TemperatureReading(stnIdentifier: String, wbanIdentifier: String, month: Int, day: Int, temperature: Double)  //, uniqueIdentifier: String)
//case class TemperatureReading(stnIdentifier: String, wbanIdentifier: Option[String], month: Option[Int], day: Option[Int], temperature: Option[Double], uniqueIdentifier: Option[String])
