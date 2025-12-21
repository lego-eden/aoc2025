import scala.collection.mutable.HashSet as MutSet
import scala.collection.mutable.Queue as MutQueue
import scala.util.boundary, boundary.break

object day10 extends Day:

  useExample = false

  type Lights = Int // bits represent each light
  extension (array: Lights)
    inline def toggle(lights: Lights): Lights = array ^ lights

  case class Machine(target: Lights, buttons: Vector[Lights], joltages: Vector[Long]):
    def reachTarget(): Option[Long] = boundary:
      val seen = MutSet.empty[Lights]
      val queue = MutQueue[(Lights, Long)]((0, 0))
      while queue.nonEmpty do
        val (lights, dist) = queue.dequeue()
        if lights == target then break(Some(dist))
        if !seen(lights) then
          seen += lights
          for
            next <- buttons.map(lights.toggle)
            if !seen(next) 
          do
            queue.enqueue((next, dist+1))
      end while

      None
    end reachTarget

    override def toString: String =
      val targetBin = target.toBinaryString
      val buttonsBin = buttons.map(_.toBinaryString).mkString(",")
      s"Machine([$targetBin], $buttonsBin, {${joltages.mkString(",")}})"
  end Machine

  def parse(machines: IndexedSeq[String]): IndexedSeq[Machine] =
    machines.collect:
      case s"[$target] $buttons {$joltages}" =>
        val targetParsed = target.zipWithIndex.foldLeft(0):
          case (acc, ('.', _)) => acc
          case (acc, ('#', i)) => acc | (0b1 << i)
          case _ => sys.error("unexpected char")
        val buttonsParsed =
          buttons.split(' ')
            .map(_.tail.init.split(',')
              .map(_.toInt)
              .foldLeft(0)((acc, next) => acc | (1 << next))
            ).toVector
        val joltagesParsed =
          joltages.split(',').map(_.toLong).toVector
        
        Machine(targetParsed, buttonsParsed, joltagesParsed)
  
  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines).map(_.reachTarget().get).sum
  def partTwo(lines: IndexedSeq[String]): Long = 0L

end day10
