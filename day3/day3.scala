object day3 extends Day:

  useExample = false

  def solve(lines: IndexedSeq[String], numBatteries: Int): Long =
    lines.map(bank =>
      maxJolt(numBatteries):
        bank.map(_.asDigit.toLong)
    ).sum

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

  def partOne(lines: IndexedSeq[String]): Long = solve(lines, 2)

  def partTwo(lines: IndexedSeq[String]): Long = solve(lines, 12)
  
end day3
