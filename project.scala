//> using scala 3.7
//> using dep com.lihaoyi::os-lib::0.11.6
//> using dep org.ojalgo:ojalgo:56.2.0
//> using options -deprecation -Wall

import java.nio.file.NoSuchFileException

trait Day:
  var useExample = false

  def readLines(path: os.ReadablePath): IndexedSeq[String] =
    try
      os.read.lines(path)
    catch
      case _: NoSuchFileException =>
        println(s"${path.toString} does not exist")
        sys.exit(-1)

  def partOne: Long | BigInt = partOne(if useExample then example1 else input)
  def partTwo: Long | BigInt = partTwo(if useExample then example2 else input)

  def partOne(lines: IndexedSeq[String]): Long | BigInt
  def partTwo(lines: IndexedSeq[String]): Long | BigInt

  lazy val input: IndexedSeq[String] =
    readLines(os.pwd / toString / "input.txt")

  lazy val example: os.ReadablePath =
    os.pwd / toString / "example.txt"

  lazy val example1 =
    val p = os.pwd / toString / "example1.txt"
    readLines(if os.exists(p) then p else example)
  lazy val example2 =
    val p = os.pwd / toString / "example2.txt"
    readLines(if os.exists(p) then p else example)

  final override def toString: String = super.toString.takeWhile(_ != '$')

  def main(args: Array[String]): Unit =
    println(s"partOne: ${partOne.toString}")
    println(s"partTwo: ${partTwo.toString}")

end Day

