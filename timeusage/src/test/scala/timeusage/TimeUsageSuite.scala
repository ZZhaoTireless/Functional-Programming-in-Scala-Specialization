package timeusage

import org.apache.spark.sql.SparkSession
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}



@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .config("spark.master", "local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  val summed =
    Seq(
      TimeUsageRow("working", "female", "active", 7.667, 0.62, 5.153),
      TimeUsageRow("working", "male", "elder", 11.002, 2.99999, 5.7111),
      TimeUsageRow("working", "female", "active", 7.667, 0.62, 5.153),
      TimeUsageRow("not working", "male", "young", 4.42, 0.53, 5.7),
      TimeUsageRow("working", "male", "elder", 11.002, 2.99999, 5.7111),
      TimeUsageRow("working", "female", "young", 13.9501, 8.97, 0.0444),
      TimeUsageRow("not working", "male", "young", 4.42, 0.53, 5.7),
      TimeUsageRow("not working", "male", "young", 4.42, 0.53, 5.7),
      TimeUsageRow("not working", "male", "young", 4.42, 0.53, 5.7),
      TimeUsageRow("working", "male", "elder", 11.002, 2.99999, 5.7111),
      TimeUsageRow("working", "female", "young", 13.9501, 8.97, 0.0444),
      TimeUsageRow("working", "male", "elder", 11.002, 2.99999, 5.7111),
      TimeUsageRow("working", "female", "young", 13.9501, 8.97, 0.0444),
      TimeUsageRow("not working", "male", "young", 4.42, 0.53, 5.7),
      TimeUsageRow("working", "male", "elder", 11.002, 2.99999, 5.7111),
      TimeUsageRow("working", "female", "young", 13.9501, 8.97, 0.0444),
      TimeUsageRow("working", "female", "active", 7.667, 0.62, 5.153),
      TimeUsageRow("working", "female", "young", 13.9501, 8.97, 0.0444),
      TimeUsageRow("working", "male", "elder", 11.002, 2.99999, 5.7111)
    ).toDF
  val expected =
    Seq(
      TimeUsageRow("not working", "male", "young", 4.4, 0.5, 5.7),
      TimeUsageRow("working", "female", "active", 7.7, 0.6, 5.2),
      TimeUsageRow("working", "female", "young", 14.0, 9.0, 0.0),
      TimeUsageRow("working", "male", "elder", 11.0, 3.0, 5.7)
    ).toDF

  test(">>> using DataFrame API") {
    val result = TimeUsage.timeUsageGrouped(summed)
    assert(result.collect.toSeq == expected.collect.toSeq, "result and expected dataframes should be equal")
  }

  test(">>> using SQL API") {
    val result = TimeUsage.timeUsageGroupedSql(summed)
    assert(result.collect.toSeq == expected.collect.toSeq, "result and expected dataframes should be equal")
  }

  test(">>> using Dataset API") {
    val ds = TimeUsage.timeUsageSummaryTyped(summed)
    val result = TimeUsage.timeUsageGroupedTyped(ds)

    def expDS = expected.as[TimeUsageRow]

    assert(result.collect.toSeq == expDS.collect.toSeq, "result and expected datasets should be equal")
  }

}
