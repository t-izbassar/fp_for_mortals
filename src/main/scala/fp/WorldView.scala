package fp
import scalaz.NonEmptyList

/**
  * Snapshot of our knowledge of the world.
  *
  * Aggregates the return values of all the
  * methods in the algebras and adds
  * pending field to track unfulfilled requests.
  */
final case class WorldView(
  backlog: Int,
  agents: Int,
  managed: NonEmptyList[MachineNode],
  alive: Map[MachineNode, Epoch],
  pending: Map[MachineNode, Epoch],
  time: Epoch
)
