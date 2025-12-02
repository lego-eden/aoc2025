import scala.collection.immutable.NumericRange.Inclusive as InclRange

object day2 extends Day:

  useExample = false

  extension (n: Long)
    def isDuplicate: Boolean =
      val nStr = n.toString
      val nd = nStr.length

      nd % 2 == 0 && nStr.take(nd / 2) * 2 == nStr

    def isRepeat: Boolean =
      val nStr = n.toString
      val nd = nStr.length
      (1 to nd / 2)
        .filter(nd % _ == 0)
        .map(i => nStr.take(i) * (nd / i))
        .exists(_ == nStr)

  def parse(lines: IndexedSeq[String]): Iterator[InclRange[Long]] =
    lines.head.split(',')
      .iterator
      .map(_.split('-'))
      .map(a => a(0).toLong to a(1).toLong )

  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines).flatMap(r => r.filter(_.isDuplicate)).sum

  def partTwo(lines: IndexedSeq[String]): Long =
    parse(lines).flatMap(r => r.filter(_.isRepeat)).sum

end day2
