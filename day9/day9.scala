object day9 extends Day:

  useExample = true

  def parse(lines: IndexedSeq[String]): IndexedSeq[(Long, Long)] =
    lines.map:
      case s"$l,$r" => (l.toLong, r.toLong)

  extension (tiles: IndexedSeq[(Long, Long)])
    def allPairs: IndexedSeq[((Long, Long),(Long, Long))] =
      for a <- tiles.indices; b <- tiles.indices.drop(a+1) yield
        (tiles(a), tiles(b))

  def area(a: (Long, Long), b: (Long, Long)): Long =
    val (ax, ay) = a
    val (bx, by) = b
    ((bx - ax).abs + 1) * ((by - ay).abs + 1)

  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines).allPairs.map(area).max
  def partTwo(lines: IndexedSeq[String]): Long = 0L

end day9
