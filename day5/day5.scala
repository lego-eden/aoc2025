import scala.util.boundary
object day5 extends Day:

  useExample = false

  case class Range(from: Long, to: Long):
    def contains(id: Long): Boolean =
      from <= id && id <= to

    def overlaps(other: Range): Boolean =
      contains(other.from) || contains(other.to)

    def merge(other: Range): IndexedSeq[Range] =
      if overlaps(other) then
        IndexedSeq(Range(
          this.from min other.from,
          this.to max other.to
        ))
      else
        IndexedSeq(this, other)

    def len: Long = to - from + 1

  def parse(lines: IndexedSeq[String]): (IndexedSeq[Range], IndexedSeq[Long]) =
    val (rangeStrings, idStrings) = lines.splitAt(lines.indexOf(""))

    val ranges = rangeStrings.collect:
      case s"$from-$to" => Range(from.toLong, to.toLong)
    val ids = idStrings.tail.map(_.toLong)
    
    (ranges, ids)

  def mergeAll(ranges: IndexedSeq[Range]): IndexedSeq[Range] =
    ranges.sortBy(_.from)
      .foldLeft(IndexedSeq.empty)((acc, range) =>
        if acc.size < 2 then acc :+ range
        else acc.init ++ acc.last.merge(range)
      )

  def partOne(lines: IndexedSeq[String]): Long =
    val (ranges, ids) = parse(lines)
    ids.count(id => ranges.exists(_.contains(id)))
  def partTwo(lines: IndexedSeq[String]): Long =
    val (ranges, _) = parse(lines)
    mergeAll(ranges).map(_.len).sum

end day5
