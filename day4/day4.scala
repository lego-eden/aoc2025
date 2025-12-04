import scala.collection.immutable.HashSet
import scala.annotation.tailrec

object day4 extends Day:
  
  useExample = false

  extension (rows: Range)
    def withCols(cols: Range) =
      for r <- rows; c <- cols yield (r, c)

  extension (grid: Vector[Vector[Char]])
    def inRange(r: Int, c: Int): Boolean =
      val (h, w) = (grid.size, grid.head.size)
      0 <= r && r < h && 0 <= c && c < w

    def rollsAround(row: Int, col: Int): Long =
      (row-1 to row+1).withCols(col-1 to col+1)
        .count: (r, c) =>
          !(r == row && c == col) && grid.inRange(r, c) && grid(r)(c) == '@'

    def rolls: Vector[(Int, Int)] =  
      grid.indices.withCols(grid.head.indices)
        .filter(grid(_)(_) == '@')
        .toVector

    def removableRolls: Vector[(Int, Int)] =
      grid.rolls.filter(grid.rollsAround(_, _) < 4L)

    def pruneRolls: Vector[Vector[Char]] =
      val removable = grid.removableRolls.to(HashSet)
      Vector.tabulate(grid.size, grid.head.size): (r, c) =>
        if removable(r, c) then '.' else grid(r)(c)

    @tailrec
    def maxPruneRolls: Vector[Vector[Char]] =
      val newRolls = grid.pruneRolls
      val diff = grid.rolls.size - newRolls.rolls.size
      if diff == 0 then newRolls
      else newRolls.maxPruneRolls
  end extension

  def partOne(lines: IndexedSeq[String]): Long =
    lines.map(_.toVector).toVector.removableRolls.size

  def partTwo(lines: IndexedSeq[String]): Long =
    val grid = lines.map(_.toVector).toVector
    val originalRolls = grid.rolls.size
    val newRolls = grid.maxPruneRolls.rolls.size
    originalRolls - newRolls

end day4
