package observatory

import java.nio.file.Paths
import java.time.LocalDate
import org.apache.spark.sql._
import org.apache.spark.sql.types.{StructField, _}
import org.apache.spark.sql.SparkSession
/**
  * 1st milestone: data extraction
  */
object Extraction {

  val spark = SparkSession.builder().appName("Observatory").config("spark.master", "local").getOrCreate()
  import spark.implicits._

  /**
    * Returns a path URI relative to the source root.
    * @param resource         file name
    * @return                 path URI
    */
  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    // open CSV file for stations - SQL method
    val stationsStg = spark.sqlContext.read
      .schema(StructType(Array(StructField("stnIdentifier", StringType),
        StructField("wbanIdentifier", StringType, nullable = true),
        StructField("latitude", DoubleType, nullable = true),
        StructField("longitude", DoubleType, nullable = true)
      ))).csv(fsPath(stationsFile))

    // open CSV file of readings for year - SQL method
    val readingsStg = spark.sqlContext.read
      .schema(StructType(Array(StructField("stnIdentifier", StringType),
        StructField("wbanIdentifier", StringType, nullable = true),
        StructField("month", IntegerType, nullable = true),
        StructField("day", IntegerType, nullable = true),
        StructField("temperature", DoubleType, nullable = true)
      ))).csv(fsPath(temperaturesFile))

    val tempCelciusColumn : Column = ((readingsStg("temperature") -32d) / 1.8d).alias("tempCelsius")

    val readings = readingsStg.select($"stnIdentifier", $"wbanIdentifier", $"month", $"day", tempCelciusColumn)

    val stations = stationsStg.select($"stnIdentifier", $"wbanIdentifier", $"latitude", $"longitude")
      .where($"latitude" =!= 9999.9d)
      .where($"longitude" =!= 9999.9d)

    // join year CSV file to stations CSV file and map to return values
    stations.createOrReplaceTempView("stations")
    readings.createOrReplaceTempView("readings")

    val readingsStations = spark.sql("""SELECT r.day, r.month, s.latitude, s.longitude, r.tempCelsius
        FROM stations s
       INNER JOIN readings r ON (s.stnIdentifier, s.wbanIdentifier) = (r.stnIdentifier, r.wbanIdentifier)""")

    val temperatures = readingsStations.select($"month", $"day", $"latitude", $"longitude", $"tempCelsius").map(
      r => (r.getAs[Int]("month"), r.getAs[Int]("day"), r.getAs[Double]("latitude"), r.getAs[Double]("longitude"),
      r.getAs[Double]("tempCelsius")))

    temperatures.collect().map(r => (LocalDate.of(year, r._1, r._2), Location(r._3, r._4), r._5))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy{ case (date: LocalDate, location: Location, temperature: Double) => location }
      .mapValues(values => values.map(_._3).sum / values.size)
  }
}
