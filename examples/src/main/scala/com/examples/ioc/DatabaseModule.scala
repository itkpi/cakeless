package com.examples.ioc

import cakeless._
import cakeless.ioc._
import cakeless.compiletime._
import com.examples.types.DbUrl
import com.examples.{Database, DbComponent, PropsComponent}
import zio.ZManaged

object DatabaseModule extends UModuleDecl[DbComponent]

object DatabaseModuleImpl
    extends ModuleDefn(
      of(DatabaseModule)
        .dependsOn(ConfigurationModule)
        .settings(
          ZManaged
            .environment[PropsComponent]
            .map(_.props.get("db-url").fold[DbUrl](ifEmpty = DbUrl("jdbc:postgresql://localhost:5432/my_db"))(DbUrl(_)))
            .flatMap { dbUrl =>
              val dbImpl: Database = new Database(dbUrl)
              ZManaged.environment[DbComponent].inject0.wire
            }
        )
    )
