import scala.annotation.tailrec
import scala.collection.mutable.HashMap as MutMap

object day7 extends Day:

  useExample = false

  def partOne(lines: IndexedSeq[String]): Long =
    val start = lines.head.indexOf('S')
    val result = lines.tail.foldLeft((0L /* splits */, Set(start) /* beams */)): (acc, line) =>
      val (splits, beams) = acc
      var additionalSplits = 0L
      val newBeams = beams.flatMap(beam =>
        if line(beam) == '^' then
          additionalSplits += 1
          Set(beam - 1, beam + 1)
        else Set(beam)
      )
      (splits + additionalSplits, newBeams)
    
    result(0) // the accumulated splits

  val timelineCache = MutMap.empty[(Int, Int), Long]
  def calculateNewTimelines(beam: Int, lines: IndexedSeq[String]): Long =
    lazy val line = lines.head
    lazy val rest = lines.tail
    if lines.isEmpty then 1L
    else
      timelineCache.getOrElseUpdate((beam, lines.size),
        if line(beam) == '^' then
          calculateNewTimelines(beam - 1, rest) + calculateNewTimelines(beam + 1, rest)
        else calculateNewTimelines(beam, rest)
      )

  def partTwo(lines: IndexedSeq[String]): Long =
    val start = lines.head.indexOf('S')
    calculateNewTimelines(start, lines.tail)
end day7