object day3 extends Day:

  useExample = false

  def parse(lines: IndexedSeq[String]): IndexedSeq[IndexedSeq[Long]] =
    lines.map(_.map(_.asDigit.toLong))

  extension (xs: Seq[Long])
    def insertMax(n: Long): Seq[Long] = xs match
      case Seq() => Seq.empty
      case Seq(x, xs*) if n >= x => n +: xs.insertMax(x)
      case other => other

  def maxJolt(len: Int)(batteryBank: Seq[Long]): Long =
    val (rest, init) = batteryBank.splitAt(batteryBank.size - len)
    rest.foldRight(init): (n, acc) =>
      acc.insertMax(n)
    .reduce(_ * 10 + _)

  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines).map(maxJolt(2)).sum

  def partTwo(lines: IndexedSeq[String]): Long =
    parse(lines).map(maxJolt(12)).sum
  
end day3
