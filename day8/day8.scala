object day8 extends Day:
  
  useExample = false
  
  extension (a: Long) inline def *-(b: Long): Long =
    val diff = a-b
    diff*diff

  class Junc(val x: Long, val y: Long, val z: Long, val id: Int, var circuit: Int):
    infix def sqDist(other: Junc): Long =
      other.x *- x + other.y *- y + other.z *- z

    override def toString: String =
      val pos = (x, y, z)
      s"$pos, id: $id, circuit: $circuit"
  
  def parse(lines: IndexedSeq[String]): Array[Junc] =
    lines.zipWithIndex.collect:
      case (s"$x,$y,$z", id) =>
        Junc(x.toLong, y.toLong, z.toLong, id, id)
    .toArray

  extension (juncs: Array[Junc])
    def distanceSorted: Vector[(Int, Int)] =
      (for a <- juncs.indices; b <- juncs.indices.drop(a+1) yield (a, b))
        .toVector
        .sortBy(juncs(_) sqDist juncs(_))
        
    def join(n: Int): Unit =
      for
        (a, b) <- juncs.distanceSorted.take(n)
      do
        juncs.connect(a, b)

    def connect(a: Int, b: Int): Unit =
      val aJunc = juncs(a)
      val bJunc = juncs(b)
      // println(s"\nConnecting $aJunc with $bJunc")
      val c = aJunc.circuit min bJunc.circuit
      for
        j <- juncs.iterator
        jc = j.circuit
        if (jc == aJunc.circuit || jc == bJunc.circuit) && jc != c
      do
        j.circuit = c
        // println(s"${juncs(a)} \t<===> ${juncs(b)}")

    def circuits(n: Int): Map[Int, Array[Junc]] =
      juncs.join(n)
      juncs.groupBy(_.circuit)
  end extension

  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines).circuits(if useExample then 10 else 1000)
      // .tapEach((i, circuit) =>
      //   println()
      //   println(circuit.size)
      //   circuit.foreach(println)
      // )
      .toVector
      .sortBy(_(1).length)
      .reverse
      .take(3)
      .tapEach((i, circuit) =>
        println()
        println(circuit.size)
        circuit.foreach(println)
      )
      .map(_(1).length)
      .product
  def partTwo(lines: IndexedSeq[String]): Long = 0L
