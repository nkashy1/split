package split

import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by nkashyap on 1/10/16.
  */
object Splitter {
  def main(args: Array[String]) = {
    val argsList = args.toSeq
    if (argsList.length != 2) {
      println("Please pass the filename and the number of lines in each of the subdivisions (except perhaps the final one) as arguments.")
      System.exit(1)
    }

    val bigFile = argsList(0)
    val bigData = Source.fromFile(bigFile).getLines
    val linesPerSubdivision = argsList(1).toInt

    subdivide(bigData, bigFile, Seq[String](), 1, linesPerSubdivision)
  }

  @tailrec
  def subdivide(bigData: Iterator[String], bigFile: String, littleData: Seq[String], lineNumber: Int, linesPerSubdivision: Int): Unit = {
    if (bigData.isEmpty) {
      if (littleData.nonEmpty) {
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
      subdivide(bigData, bigFile, Seq[String](), lineNumber + 1, linesPerSubdivision)
    } else {
      subdivide(bigData, bigFile, littleData :+ bigData.next(), lineNumber + 1, linesPerSubdivision)
    }
  }

  def writeData(data: Seq[String], outFile: String) = {
    import java.io._
    val writer = new PrintWriter(new File(outFile))
    data.foreach( writer.write )
    writer.close()
  }
}
