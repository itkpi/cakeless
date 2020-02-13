package com.examples.ioc

import cakeless._
import cakeless.module._
import com.examples.{Database, DbComponent, PropsComponent}
import com.examples.types.DbUrl
import zio.ZIO

object DatabaseModule extends UModuleDecl[DbComponent]

object DatabaseModuleImpl
    extends ModuleDefn(
      of(DatabaseModule)
        .dependsOn(ConfigurationModule)
        .settings {
          ZIO
            .access[PropsComponent](_.props.get("db-url").fold[DbUrl](ifEmpty = DbUrl("jdbc:postgresql://localhost:5432/my_db"))(DbUrl(_)))
            .flatMap { dbUrl =>
              val dbImpl: Database = new Database(dbUrl)
              ZIO.environment[DbComponent].inject0.wire
            }
        }
    )
