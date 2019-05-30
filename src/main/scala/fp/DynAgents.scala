package fp
import scalaz.Monad
import scalaz.Scalaz._
import scala.concurrent.duration.DurationInt
import scalaz.NonEmptyList

/**
  * The main business logic.
  */
trait DynAgents[F[_]] {
  def initial: F[WorldView]
  def update(old: WorldView): F[WorldView]
  def act(world: WorldView): F[WorldView]
}

/**
  * Module implementation of business logic. The module
  * depends only on other modules, algebras and pure
  * functions, and can be abstracted over F.
  */
final class DynAgentsModule[F[_]: Monad](D: Drone[F], M: Machines[F])
    extends DynAgents[F] {

  /**
    * Call all external services and aggregate their
    * results. The pending is empty at this point.
    */
  override def initial: F[WorldView] =
    for {
      db <- D.getBacklog
      da <- D.getAgents
      mm <- M.getManaged
      ma <- M.getAlive
      mt <- M.getTime
    } yield WorldView(db, da, mm, ma, Map.empty, mt)

  /**
    * Should call initial to refresh our world view,
    * preserving known pending actions.
    */
  override def update(old: WorldView) =
    for {
      snap <- initial
      changed = symdiff(old.alive.keySet, snap.alive.keySet)
      pending = (old.pending -- changed).filterNot {
        case (_, started) => (snap.time - started) >= 10.minutes
      }
      update = snap.copy(pending = pending)
    } yield update

  private def symdiff[T](a: Set[T], b: Set[T]): Set[T] =
    (a union b) -- (a intersect b)

  override def act(world: WorldView) = world match {
    case NeedsAgent(node) =>
      for {
        _ <- M.start(node)
        update = world.copy(pending = Map(node -> world.time))
      } yield update

    case Stale(nodes) =>
      nodes.foldLeftM(world) { (world, n) =>
        for {
          _ <- M.stop(n)
          update = world.copy(pending = world.pending + (n -> world.time))
        } yield update
      }

    case _ => world.pure[F]
  }

  private object NeedsAgent {

    /**
      * Returns candidate node to start. Start new node only if
      * there is no agents and pending machines available.
      */
    def unapply(world: WorldView): Option[MachineNode] = world match {
      case WorldView(backlog, 0, managed, alive, pending, _)
          if backlog > 0 && alive.isEmpty && pending.isEmpty =>
        Option(managed.head)
      case _ => None
    }
  }

  private object Stale {
    def unapply(world: WorldView): Option[NonEmptyList[MachineNode]] =
      world match {
        case WorldView(backlog, _, _, alive, pending, time) if alive.nonEmpty =>
          (alive -- pending.keys)
            .collect {
              case (n, started)
                  if backlog == 0 && (time - started).toMinutes % 60 >= 58 =>
                n
              case (n, started) if (time - started) >= 5.hours => n
            }
            .toList
            .toNel

        case _ => None
      }
  }
}
