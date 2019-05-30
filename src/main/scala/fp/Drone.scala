package fp
import scalaz.NonEmptyList

/**
  * CI Server, which receives jobs and
  * manages worker agents.
  */
trait Drone[F[_]] {
  def getBacklog: F[Int]
  def getAgents: F[Int]
}

final case class MachineNode(id: String)
trait Machines[F[_]] {
  def getTime: F[Epoch]
  def getManaged: F[NonEmptyList[MachineNode]]
  def getAlive: F[Map[MachineNode, Epoch]]
  def start(node: MachineNode): F[MachineNode]
  def stop(node: MachineNode): F[MachineNode]
}
