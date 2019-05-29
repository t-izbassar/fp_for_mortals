package fp

import scalaz._
import scalaz.Scalaz._

class Mutable(state: WorldView) {
  var started, stopped: Int = 0

  private val D: Drone[Id] = new Drone[Id] {
    override def getBacklog: Int = state.backlog
    override def getAgents: Int = state.agents
  }

  private val M: Machines[Id] = new Machines[Id] {
    override def getAlive: Map[MachineNode, Epoch] = state.alive
    override def getManaged: NonEmptyList[MachineNode] = state.managed
    override def getTime: Epoch = state.time
    override def start(node: MachineNode): MachineNode = {
      started += 1
      node
    }
    override def stop(node: MachineNode): MachineNode = {
      stopped += 1
      node
    }
  }

  val program = new DynAgentsModule[Id](D, M)
}
