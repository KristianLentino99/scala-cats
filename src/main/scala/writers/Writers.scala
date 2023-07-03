package writers

import cats.effect.IO

object Writers {

  import cats.data.Writer

  val aWriter: Writer[List[String], Int] = Writer(List("  Started something"), 1)
  val anIncreasedWriter = aWriter.map(_ + 1) //log will not change
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something") //values will not change, will change only logs
  val aCompletelyChangedWriter = aWriter.bimap(_ :+ "changed", _ + 1) //both logs and values will change
  val mapBothWriter = aWriter.mapBoth { (logs, values) =>
    (logs :+ "changed", values + 1)
  }
  val desiredValue = anIncreasedWriter.value
  val logs = anIncreasedWriter.written
  val (l, values) = anIncreasedWriter.run

  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1", "Log B2"), 15)

  val compositeWriter = for {
    valueA <- writerA
    valueB <- writerB
  } yield valueA + valueB

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if(n <= 0) Writer(Vector("Starting...."), 0)
    else countAndLog(n - 1).mapBoth { (logs, value) =>
      (logs :+ s"Added $n", value + n)
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if(n <= 0) Writer(Vector.empty, 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWithLogs(n - 1)
      _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n


  }
  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    println(countAndLog(10).run)

    /**
     * The Writer approach allow us to keep the logs together when operating in a multi-threaded environment
     * Otherwise with a simple logging approach we would have logs from different threads mixed together
     */
    println(sumWithLogs(10).written.foreach(println))
  }
}
