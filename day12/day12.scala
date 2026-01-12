object day12 extends Day:

  case class Grid(w: Int, h: Int, numShapes: Vector[Int]):
    def isValid(areas: Vector[Int]): Boolean =
      lazy val naivePlacement =
        (w / 3) * (h / 3) >= numShapes.sum
      lazy val naiveImpossible =
        w * h < numShapes.zip(areas).map(_*_).sum
      
      // it should be one of these or this is really hard
      assert(naiveImpossible || naivePlacement)
      naivePlacement && !naiveImpossible

  def parse(lines: IndexedSeq[String]): (Vector[Int], Vector[Grid]) =
    val (shapeStrings :+ gridsString) =
      lines.mkString("\n").split("\n\n").toVector: @unchecked
    
    val shapes = shapeStrings.map(_.count(_=='#'))
    val grids = gridsString.split('\n').collect:
      case s"${w}x${h}: $rest" =>
        Grid(w.toInt, h.toInt, rest.split(' ').map(_.toInt).toVector)

    (shapes, grids.toVector)

  def partOne(lines: IndexedSeq[String]): Long =
    val (areas, grids) = parse(lines)
    grids.count(_.isValid(areas))
  def partTwo(lines: IndexedSeq[String]): Long =
    println("done!")
    sys.exit(0)

end day12
