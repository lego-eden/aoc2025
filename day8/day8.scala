import scala.util.boundary
object day8 extends Day:
  
  useExample = false
  
  extension (a: Int) inline def *-(b: Int): Int =
    val diff = a-b
    diff*diff

  case class Junc(x: Int, y: Int, z: Int, id: Int, circuit: Int):
    infix def sqDist(other: Junc): Int =
      val Junc(ox, oy, oz, _, _) = other
      ox *- x + oy *- y + oz *- z
  
  def parse(lines: IndexedSeq[String]): Vector[Junc] =
    lines.zipWithIndex.collect:
      case (s"$x,$y,$z", id) =>
        Junc(x.toInt, y.toInt, z.toInt, id, id)
    .toVector

  extension (juncs: Vector[Junc])
    def distanceSorted: Vector[(Int, Int)] =
      (for a <- juncs; b <- juncs if a != b yield (a, b))
        .sortBy(_ sqDist _).map((a, b) => (a.id, b.id))
        
    def joined(n: Int): Vector[Junc] = boundary:
      var connections = 0
      juncs.distanceSorted.foldLeft(juncs){ case (acc, (a, b)) =>
        if acc(a).circuit != acc(b).circuit then
          val result = acc.connect(a, b)
          connections += 1
          if connections >= n then boundary.break(acc)
          result
        else
          acc
      }

    def connect(a: Int, b: Int): Vector[Junc] =
      val aJunc = juncs(a)
      val bJunc = juncs(b)
      val c = aJunc.circuit min bJunc.circuit
      juncs.map(j =>
        if j.circuit == aJunc.circuit || j.circuit == bJunc.circuit then
          j.copy(circuit=c)
        else
          j
      )

    def circuits(n: Int): Map[Int, Vector[Junc]] =
      juncs.joined(n).groupBy(_.circuit)
  end extension

  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines).circuits(if useExample then 10 else 1000)
      // .tapEach((i, circ) => println(circ))
      .map((_, circuit) => circuit.size)
      .toVector
      .sorted
      .reverse
      .take(3)
      .product
  def partTwo(lines: IndexedSeq[String]): Long = 0L
