package split

import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by nkashyap on 1/10/16.
  */
object Splitter {
  def main(args: Array[String]) = {
    val argsList = args.toSeq
    if (argsList.length != 2 && argsList.length != 3) {
      println("Please pass the filename and the number of lines in each of the subdivisions (except perhaps the final one) as arguments. Optionally pass 1 to specify that there is metadata at the head of the file that should be copied to each of the parts.")
      System.exit(1)
    }

    val bigFile = argsList(0)
    val bigData = Source.fromFile(bigFile).getLines
    val linesPerSubdivision = argsList(1).toInt
    val metadata = if (argsList.length == 3) {
      Some(bigData.next())
    } else {
      None
    }

    subdivide(bigData, bigFile, newContainer(metadata), 1, linesPerSubdivision, metadata)
  }

  @tailrec
  def subdivide(bigData: Iterator[String], bigFile: String, littleData: Seq[String], lineNumber: Int, linesPerSubdivision: Int, metadata: Option[String]): Unit = {
    if (bigData.isEmpty) {
      if (littleData.length > newContainer(metadata).length) {
        val partNumber = if ( (lineNumber - 1) % linesPerSubdivision == 0 ) {
          (lineNumber - 1)/linesPerSubdivision
        } else {
          (lineNumber - 1)/linesPerSubdivision + 1
        }
        writeData(littleData, bigFile + "." + partNumber)
      }
      return
    }
    if (lineNumber % linesPerSubdivision == 0) {
      writeData(littleData :+ bigData.next(), bigFile + "." + (lineNumber/linesPerSubdivision).toString)
      subdivide(bigData, bigFile, newContainer(metadata), lineNumber + 1, linesPerSubdivision, metadata)
    } else {
      subdivide(bigData, bigFile, littleData :+ bigData.next(), lineNumber + 1, linesPerSubdivision, metadata)
    }
  }

  def writeData(data: Seq[String], outFile: String) = {
    import java.io._
    val writer = new PrintWriter(new File(outFile))
    writer.write(data.mkString("\n"))
    writer.close()
  }

  def newContainer(metadata: Option[String]) = metadata match {
    case Some(data) => Seq[String](data)
    case _ => Seq[String]()
  }
}
