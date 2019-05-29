package fp

import org.scalatest._
import Data._

class MutableSpec extends FlatSpec with Matchers {

  "Business Logic" should "generate initial world view" in {
    val mutable = new Mutable(needsAgents)

    mutable.program.initial shouldBe needsAgents
  }

  it should "remove changed nodes from pending" in {
    val world = WorldView(0, 0, managed, Map(node1 -> time3), Map.empty, time3)
    val mutable = new Mutable(world)

    val old =
      world.copy(alive = Map.empty, pending = Map(node1 -> time2), time = time2)
    mutable.program.update(old) shouldBe world
  }

  it should "request agents when needed" in {
    val mutable = new Mutable(needsAgents)

    val expected = needsAgents.copy(pending = Map(node1 -> time1))

    mutable.program.act(needsAgents) shouldBe expected

    mutable.stopped shouldBe 0
    mutable.started shouldBe 1
  }
}
