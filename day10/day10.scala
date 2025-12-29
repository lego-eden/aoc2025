import scala.collection.mutable.HashSet as MutSet
import scala.collection.mutable.Queue as MutQueue
import scala.util.boundary, boundary.break
import org.ojalgo.optimisation.ExpressionsBasedModel;

object day10 extends Day:

  useExample = false

  type Lights = Int // bits represent each light
  extension (array: Lights)
    inline def toggle(lights: Lights): Lights = array ^ lights

    def toVector(len: Int): Vector[Int] =
      var jolts = Vector.fill(len)(0)
      var n = array
      for i <- 0 until len do
        jolts = jolts.updated(i, n & 0b1)
        n >>= 1
      jolts

  extension (v: Vector[Double])
    def +(u: Vector[Double]): Vector[Double] =
      v.zip(u).map(_ + _)
    
    def *(scalar: Double): Vector[Double] =
      v.map(_ * scalar)

  case class Machine(target: Lights, buttons: Vector[Lights], joltages: Vector[Int]):

    lazy val btnVecs = buttons.map(_.toVector(joltages.length))
    lazy val btnConstraints =
      btnVecs.map(btn =>
        btn.indices.filter(btn(_) == 1).map(joltages).min
      )

    def reachTarget(): Option[Long] = boundary:
      val seen = MutSet.empty[Lights]
      val queue = MutQueue[(Lights, Long)]((0, 0))
      while queue.nonEmpty do
        val (lights, dist) = queue.dequeue()
        if lights == target then break(Some(dist))
        for
          next <- buttons.map(lights.toggle)
          if !seen(next) 
        do
          seen += next
          queue.enqueue((next, dist+1))
      end while

      None
    end reachTarget

    def reachJoltages(): Double =
      val model = ExpressionsBasedModel()
      val vars = Vector.fill(buttons.length)(
        model.addVariable().lower(0).integer(true).weight(1)
      )
      btnVecs.transpose.zip(joltages).foreach((row, j) =>
        val c = model.addExpression().level(j)
        row.zip(vars).collect{ case (1, x) => x }
          .foreach(c.set(_, 1))
      )
      model.minimise().getValue()
    end reachJoltages
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
          joltages.split(',').map(_.toInt).toVector
        
        Machine(targetParsed, buttonsParsed, joltagesParsed)
  
  def partOne(lines: IndexedSeq[String]): Long =
    parse(lines).map(_.reachTarget().get).sum
  def partTwo(lines: IndexedSeq[String]): Long = 
    parse(lines).map(_.reachJoltages().round).sum

end day10
