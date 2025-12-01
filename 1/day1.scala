import scala.math.floorMod

@main def day1 =
  lazy val wd = os.pwd/"1"
  lazy val testdata = wd/"test.txt"
  lazy val datadata = wd/"data.txt"
  
  val dialstart = 50

  def parse(file: os.ReadablePath): Iterator[Int] =
    os.read.lines(file).init
      .iterator
      .map:
        case s"R$n" => n.toInt
        case s"L$n" => -n.toInt

  def part1(file: os.ReadablePath) =
    parse(file)
      .scanLeft(dialstart): (acc, offset) =>
        floorMod(acc + offset, 100)
      .count(_ == 0)

  def part2(file: os.ReadablePath) =
    val res = parse(file).foldLeft((dialstart, 0)):
      case ((dial, zerocount), offset) =>
        lazy val extraCount = (offset / 100).abs
        val newDial = floorMod(dial + offset, 100)
        if newDial == 0 || dial != 0 && (dial - newDial).sign == offset.sign then
          (newDial, zerocount + 1 + extraCount)
        else
          (newDial, zerocount + extraCount) 
    res(1)

  println(part2(datadata))
end day1
