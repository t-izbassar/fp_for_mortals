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

  it should "not request agents when pending" in {
    val world = needsAgents.copy(pending = Map(node1 -> time2, node2 -> time2))
    val mutable = new Mutable(world)

    mutable.program.act(world) shouldBe world

    mutable.stopped shouldBe 0
    mutable.started shouldBe 0
  }

  it should "not shutdown agents if nodes are too young" in {
    val world = WorldView(0, 5, managed, Map(node1 -> time1), Map.empty, time2)
    val mutable = new Mutable(world)

    mutable.program.act(world) shouldBe world

    mutable.stopped shouldBe 0
  }

  it should "shutdown agents when there is no backlog" in {
    val world = WorldView(
      0,
      5,
      managed,
      Map(node1 -> time1, node2 -> time1),
      Map.empty,
      time3
    )
    val mutable = new Mutable(world)

    val expected =
      world.copy(pending = Map(node1 -> time3, node2 -> time3), time = time3)

    mutable.program.act(world) shouldBe expected

    mutable.stopped shouldBe 2
    mutable.started shouldBe 0
  }

  it should "not shutdown agents if there are pending actions" in {
    val world = WorldView(
      0,
      5,
      managed,
      Map(node1 -> time1, node2 -> time1),
      Map(node1 -> time2, node2 -> time2),
      time3
    )
    val mutable = new Mutable(world)

    mutable.program.act(world) shouldBe world

    mutable.stopped shouldBe 0
    mutable.started shouldBe 0
  }

  it should "shutdown agents if they are too old" in {
    val world = WorldView(
      5,
      5,
      managed,
      Map(node1 -> time1, node2 -> time1),
      Map.empty,
      time4
    )
    val mutable = new Mutable(world)

    val expected =
      world.copy(pending = Map(node1 -> time4, node2 -> time4), time = time4)

    mutable.program.act(world) shouldBe expected

    mutable.stopped shouldBe 2
    mutable.started shouldBe 0
  }

  it should "ignore unresponsive pending actions during update" in {
    val old = WorldView(5, 5, managed, Map.empty, Map(node2 -> time1), time3)
    val initial = WorldView(5, 5, managed, Map.empty, Map.empty, time2)
    val mutable = new Mutable(initial)

    mutable.program.update(old) shouldBe initial
  }
}
