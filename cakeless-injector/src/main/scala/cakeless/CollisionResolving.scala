package cakeless

/**
  * Phantom type providing strategy for handling name collisions.
  * For instance: {{{
  *   import zio._
  *   trait Database {
  *     val db: Database.Service
  *   }
  *   object Database {
  *     trait Service { def run(query: Query): IO[DbException, ResultSet] }
  *     class Live(...) extends Service { ... }
  *   }
  *
  *   object Main extends App {
  *     def run(args: List[String] = {
  *       val postgresDb: Database.Service = ???
  *       val oracleDb: Database.Service = ???
  *
  *       ZIO.accessM[Database](_.db.run(...))
  *         .injectPrimary
  *         .wire
  *         .orDie
  *         .as(1) // raises compile-time error by default because of several Database.Service instances in scope
  *     }
  *   }
  * }}}
  * */
sealed trait CollisionResolving
object CollisionResolving {

  /** @note - Default strategy, will raise compile time error */
  sealed trait Raise extends CollisionResolving
  case object Raise  extends Raise

  /** @note - will just generate a warning, not an error */
  sealed trait Warn extends CollisionResolving
  case object Warn  extends Warn

  /**
    * @note - cakeless will resolve it for you ;)
    * */
  sealed trait Auto extends CollisionResolving
  case object Auto  extends Auto

  type Default = Auto
}
