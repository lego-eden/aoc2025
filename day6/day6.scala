object day6 extends Day:

  extension [A](xs: IndexedSeq[A])
    def splitWhen(when: A => Boolean): IndexedSeq[IndexedSeq[A]] =
      xs.foldLeft(IndexedSeq(IndexedSeq.empty[A])):
        case (acc, element) if when(element) =>
          acc :+ IndexedSeq.empty[A]
        case (acc, element) =>
          acc.init :+ (acc.last :+ element)

  extension (ops: Array[String])
    def toOps = ops.map[(Long, Long) => Long]:
      case "+" => _ + _
      case "*" => _ * _
      case _   => sys.error("unexpected operator")

  def partOne(lines: IndexedSeq[String]): Long =
    val splitLines = lines.map(_.split(' ').map(_.strip).filter(_.nonEmpty))
    val numbers = splitLines.init.map(_.map(_.toLong)).transpose
    val operators = splitLines.last.toOps
    numbers.zip(operators).map((nums, op) => nums.reduce(op)).sum

  def partTwo(lines: IndexedSeq[String]): Long =
    val numbers =
      lines.init.transpose
        .map(_.mkString.strip)
        .splitWhen(_.isEmpty)
        .map(_.map(_.toLong))
    val operators =
      lines.last.split(' ').map(_.strip).filter(_.nonEmpty).toOps

    numbers.zip(operators).map((nums, op) => nums.reduce(op)).sum

end day6
