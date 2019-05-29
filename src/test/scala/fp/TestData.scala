package fp
import scalaz.NonEmptyList

object Data {
  val node1 = MachineNode("123d1af")
  val node2 = MachineNode("550c494")
  val managed = NonEmptyList(node1, node2)

  import Epoch._
  val time1: Epoch = epoch"2017-03-03T18:07:00Z"
  val time2: Epoch = epoch"2017-03-03T18:59:00Z"
  val time3: Epoch = epoch"2017-03-03T19:06:00Z"
  val time4: Epoch = epoch"2017-03-03T23:07:00Z"

  val needsAgents = WorldView(5, 0, managed, Map.empty, Map.empty, time1)
}
