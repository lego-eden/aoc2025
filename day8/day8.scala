import scala.collection.mutable.Map as MutMap
import scala.collection.mutable.Set as MutSet
import scala.collection.mutable.Buffer
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
      // println(s"union $a $b")
      if nu.size < nv.size then
        nu.parent = v
        nv.size = nu.size + nv.size
        // println(s"now parent=$v size=${nv.size}")
      else
        nv.parent = u
        nu.size = nu.size + nv.size
        // println(s"now parent=$u size=${nu.size}")


  extension (a: Long) inline def *-(b: Long): Long =
    val diff = a-b
    diff*diff

  extension (a: Pos)
    infix def sqDist(b: Pos): Long =
      a.x *- b.x + a.y *- b.y + a.z *- b.z

  extension (positions: Vector[Pos])
    def connections: Vector[(NodeID, NodeID)] =
      val allEdges =
        for
          a <- positions.indices
          b <- positions.indices.drop(a+1)
        yield
          (a, b)
      allEdges.toVector
        .sortBy((a, b) => positions(a) sqDist positions(b))
        // .tapEach((a, b) =>
        //   println(s"${positions(a)} ${positions(b)}")
        // )

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
  def partTwo(lines: IndexedSeq[String]): Long = 0L
