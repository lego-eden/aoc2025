import scala.math.floorMod

object day1 extends Day:
  val dialstart = 50

  def parse(lines: IndexedSeq[String]): Iterator[Int] =
    lines.init
      .iterator
      .map:
        case s"R$n" => n.toInt
        case s"L$n" => -n.toInt

  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines)
      .scanLeft(dialstart): (acc, offset) =>
        floorMod(acc + offset, 100)
      .count(_ == 0)

  def partTwo(lines: IndexedSeq[String]): Long =
    val res = parse(lines).foldLeft((dialstart, 0)):
      case ((dial, zerocount), offset) =>
        lazy val extraCount = (offset / 100).abs
        val newDial = floorMod(dial + offset, 100)
        if newDial == 0 || dial != 0 && (dial - newDial).sign == offset.sign then
          (newDial, zerocount + 1 + extraCount)
        else
          (newDial, zerocount + extraCount) 
    res(1)

end day1
