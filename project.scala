//> using scala 3.7
//> using dep com.lihaoyi::os-lib::0.11.6

trait Day:
  var useExample = false

  def partOne: Long | BigInt = partOne(if useExample then example else input)
  def partTwo: Long | BigInt = partTwo(if useExample then example else input)

  def partOne(lines: IndexedSeq[String]): Long | BigInt
  def partTwo(lines: IndexedSeq[String]): Long | BigInt

  lazy val input: IndexedSeq[String] =
    os.read.lines(os.pwd / toString / "input.txt")

  lazy val example: IndexedSeq[String] =
    os.read.lines(os.pwd / toString / "example.txt")

  final override def toString: String = super.toString.takeWhile(_ != '$')

  def main(args: Array[String]): Unit =
    println(s"partOne: $partOne")
    println(s"partTwo: $partTwo")

end Day

