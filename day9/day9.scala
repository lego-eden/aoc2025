object day9 extends Day:

  useExample = false

  case class LineSegment(at: Long, start: Long, stop: Long):

    lazy val min = start min stop
    lazy val max = start max stop

    def containsInclUpper(idx: Long): Boolean =
      min < idx && idx <= max
    def containsInclLower(idx: Long): Boolean =
      min <= idx && idx < max
    def containsExcl(idx: Long): Boolean =
      min < idx && idx < max
    
    // ort is a line segment that is orthogonal to this one
    def intersectsUpper(ort: LineSegment): Boolean =
      ort.containsInclUpper(at) && containsInclUpper(ort.at)

    def intersectsLower(ort: LineSegment): Boolean =
      ort.containsInclLower(at) && containsInclLower(ort.at)

    def intersectsExcl(ort: LineSegment): Boolean =
      ort.containsExcl(at) && containsExcl(ort.at)

    def intersectsAny(ortLines: IndexedSeq[LineSegment]): Boolean =
      ortLines.exists(intersectsExcl)

  case class Polygon(corners: Set[(Long, Long)], vLines: IndexedSeq[LineSegment], hLines: IndexedSeq[LineSegment]):
    
    lazy val rightmost: Long = vLines.map(_.at).max
    lazy val leftmost: Long = vLines.map(_.at).min
    lazy val lowermost: Long = hLines.map(_.at).max
    lazy val uppermost: Long = hLines.map(_.at).min
    
    def contains(point: (Long, Long)): Boolean =
      lazy val (x, y) = point
      lazy val raycast = LineSegment(y, x, (rightmost+1) max x)
      // count intersections with the vertical lines of the polygon
      // if the count is odd then the point is inside
      lazy val vIntersectsUpper = vLines.count(raycast.intersectsUpper)
      lazy val vIntersectsLower = vLines.count(raycast.intersectsLower)
      corners(point) || (vIntersectsUpper) % 2 != 0 || (vIntersectsLower % 2 != 0)

    // the corners we pass in are already contained since they are red tiles
    // we only need to check the remaining two corners
    def contains(corner1: (Long, Long), corner2: (Long, Long)): Boolean =
      import LineSegment as L
      lazy val (ax, ay) = corner1
      lazy val (bx, by) = corner2

      contains((ax, by)) && contains((bx, ay)) &&
      !L(ax, ay, by).intersectsAny(hLines) &&
      !L(bx, ay, by).intersectsAny(hLines) &&
      !L(ay, ax, bx).intersectsAny(vLines) &&
      !L(by, ax, bx).intersectsAny(vLines)

    def prettyPrint(): Unit =
      for y <- (uppermost - 2) until (lowermost + 3) by 350 do
        for x <- (leftmost - 1) until (rightmost + 2) by 350 do
          print(
            if contains((x, y)) then 'X'
            else '.'
          )
          print(' ')
        println()

  def parse(lines: IndexedSeq[String]): IndexedSeq[(Long, Long)] =
    lines.map:
      case s"$l,$r" => (l.toLong, r.toLong)

  extension (tiles: IndexedSeq[(Long, Long)])
    def allPairs: IndexedSeq[((Long, Long),(Long, Long))] =
      for a <- tiles.indices; b <- tiles.indices.drop(a+1) yield
        (tiles(a), tiles(b))

    def polygon: Polygon =
      val (vLines, hLines) = (tiles :+ tiles.head).sliding(2)
        .map(xs => (xs(0), xs(1)))
        .foldLeft(IndexedSeq.empty[LineSegment], IndexedSeq.empty[LineSegment]):
          case ((v, h), ((ax, ay), (bx, by))) =>
            if ay == by then // horizonal line
              (v, h :+ LineSegment(ay, ax, bx))
            else if ax == bx then // vertical line
              (v :+ LineSegment(ax, ay, by), h)
            else
              sys.error("Unexcpected input")
      Polygon(tiles.toSet, vLines, hLines)
    end polygon

  def area(a: (Long, Long), b: (Long, Long)): Long =
    val (ax, ay) = a
    val (bx, by) = b
    ((bx - ax).abs + 1) * ((by - ay).abs + 1)

  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines).allPairs.map(area).max
  def partTwo(lines: IndexedSeq[String]): Long =
    val corners = parse(lines)
    val poly = corners.polygon
    corners.allPairs
      .filter((c1, c2) => poly.contains(c1, c2))
      .map(area)
      .max

end day9
