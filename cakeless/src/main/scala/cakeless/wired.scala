package cakeless

import scala.annotation.StaticAnnotation

/**
  * Used to explicitly indicate the dependency for .wire macro
  * when several instances of the same type are present
  *
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
  *       @wired val postgresDb: Database.Service = ???
  *       val oracleDb: Database.Service = ???
  *
  *       ZIO.accessM[Database](_.db.run(...))
  *         .injectPrimary
  *         .wire
  *         .orDie
  *         .as(1) // will use postgresDb
  *     }
  *   }
  * }}}
  * */
class wired extends StaticAnnotation
