import scala.collection.immutable.HashMap
import scala.collection.mutable.HashMap as MutMap

object day11 extends Day:

  useExample = false

  case class Node(id: String, neighbors: Vector[String])
  type Graph = HashMap[String, Node]

  extension (g: Graph)
    def pathsBetween(from: String, to: String): BigInt =
      val cache = MutMap.empty[String, BigInt]

      def pathsBetweenRec(from: String): BigInt =
        def pathsBetweenImpl(from: String): BigInt =
          if from == to then 1
          else
            g(from).neighbors.map(id => pathsBetweenRec(id)).sum
        cache.getOrElseUpdate(from, pathsBetweenImpl(from))

      pathsBetweenRec(from)
    end pathsBetween

    def pathsThrough(start: String, through: String, throughRest: String*): BigInt =
      (start +: through +: throughRest).sliding(2)
        .map(pair => pathsBetween(pair(0), pair(1)))
        .reduce(_ * _)

  def parse(lines: IndexedSeq[String]): Graph =
    lines.map:
      case s"$id: $neighborStrings" =>
        val neighbors = neighborStrings.split(' ').toVector
        id -> Node(id, neighbors)
    .to(HashMap) + ("out" -> Node("out", Vector.empty))

  def partOne(lines: IndexedSeq[String]): BigInt =
    parse(lines).pathsBetween("you", "out")

  def partTwo(lines: IndexedSeq[String]): BigInt =
    val graph = parse(lines)
    graph.pathsThrough("svr", "fft", "dac", "out")
    + graph.pathsThrough("svr", "dac", "fft", "out")

end day11
