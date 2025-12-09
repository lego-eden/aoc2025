import scala.util.boundary

object day8 extends Day:
  
  useExample = false
  
  type Pos = (x: Long, y: Long, z: Long)
  type NodeID = Int

  class Node(var parent: NodeID, var size: Int):
    override def toString: String = s"(parent=$parent, size=$size)"

  def createGraph(lines: IndexedSeq[String], n: Int): Array[Node] =
    val emptyGraph = Array.tabulate(lines.length)(i => Node(i, 1))
    val connections = parse(lines).connections.take(n)

    for (a, b) <- connections do
      emptyGraph.union(a, b)

    emptyGraph

  def calcWallDist(lines: IndexedSeq[String]): Long =
    val emptyGraph = Array.tabulate(lines.length)(i => Node(i, 1))
    val positions = parse(lines)
    val connections = positions.connections
    val sortedPositions = connections.map((a, b) => (positions(a), positions(b)))
    val i = boundary:
      for ((a, b), i) <- connections.zipWithIndex do
        emptyGraph.union(a, b)
        if emptyGraph(emptyGraph.find(a)).size == emptyGraph.length then
          boundary.break(i)
      connections.length - 1

    val ((x1,_,_), (x2,_,_)) = sortedPositions(i)
    x1 * x2

  extension (graph: Array[Node])
    def find(node: NodeID): NodeID =
      // find the root parent
      var p = node
      while graph(p).parent != p do
        p = graph(p).parent
      // replace the old parents with the root
      var v = node
      while graph(v).parent != v do
        val tmp = graph(v).parent
        graph(v).parent = p
        v = tmp
      p

    def union(a: NodeID, b: NodeID) = boundary:
      val (u, v) = (graph.find(a), graph.find(b))
      if u == v then boundary.break()
      val (nu, nv) = (graph(u), graph(v))
      if nu.size < nv.size then
        nu.parent = v
        nv.size = nu.size + nv.size
      else
        nv.parent = u
        nu.size = nu.size + nv.size

  extension (a: Long) inline def *-(b: Long): Long =
    val diff = a-b
    diff*diff

  extension (a: Pos)
    infix def sqDist(b: Pos): Long =
      a.x *- b.x + a.y *- b.y + a.z *- b.z

  extension (positions: Vector[Pos])
    def distanceSort: Vector[(Pos, Pos)] =
      positions.connections.map((a, b) => (positions(a), positions(b)))

    def allConnections: Vector[(NodeID, NodeID)] =
      val allEdges =
        for
          a <- positions.indices
          b <- positions.indices.drop(a+1)
        yield
          (a, b)
      allEdges.toVector

    def connections: Vector[(NodeID, NodeID)] =
      positions.allConnections
        .sortBy((a, b) => positions(a) sqDist positions(b))
  end extension

  def parse(lines: IndexedSeq[String]): Vector[Pos] =
    lines.collect:
      case s"$x,$y,$z" =>
        (x.toLong, y.toLong, z.toLong)
      case _ => sys.error("unexpected line")
    .toVector

  def partOne(lines: IndexedSeq[String]): Long =
    val graph = createGraph(lines, if useExample then 10 else 1000)
    graph.map(n => n.parent).distinct
      .map(n => graph(n).size)
      .sorted
      .reverse
      .take(3)
      .product

  def partTwo(lines: IndexedSeq[String]): Long =
    calcWallDist(lines)

end day8
