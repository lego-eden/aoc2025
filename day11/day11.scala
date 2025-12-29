import scala.collection.immutable.{HashSet, HashMap}

object day11 extends Day:

  case class Node(id: String, neighbors: Vector[String])
  type Graph = HashMap[String, Node]
  type Path = Vector[String]

  extension (g: Graph)
    def dfs(
        currentNode: String,
        targetNode: String,
        currentPath: Path,
        visited: HashSet[String]
    ): Vector[Path] =
      if currentNode == targetNode then
        Vector(currentPath :+ currentNode)
      else
        g(currentNode).neighbors.flatMap(n =>
          dfs(n, targetNode, currentPath :+ n, visited + n)
        )

    def allPaths: Vector[Path] =
      dfs("you", "out", Vector.empty, HashSet.empty)

  def parse(lines: IndexedSeq[String]): Graph =
    lines.map:
      case s"$id: $neighborStrings" =>
        val neighbors = neighborStrings.split(' ').toVector
        id -> Node(id, neighbors)
    .to(HashMap)

  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines).allPaths.length
  def partTwo(lines: IndexedSeq[String]): Long = 0

end day11
