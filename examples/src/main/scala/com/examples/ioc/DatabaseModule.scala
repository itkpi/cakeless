package com.examples.ioc

import cakeless._
import cakeless.compiletime._
import cakeless.ioc._
import com.examples.types.DbUrl
import com.examples.{Database, DbComponent, PropsComponent}
import zio.ZIO

object DatabaseModule extends UModuleDecl[DbComponent]

object DatabaseModuleImpl
    extends ModuleDefn(
      of(DatabaseModule)
        .dependsOn(ConfigurationModule)
        .settings(
          ZIO
            .access[PropsComponent] { props =>
              val dbUrl = props.props
                .get("db-url")
                .fold[DbUrl](
                  ifEmpty = DbUrl("jdbc:postgresql://localhost:5432/my_db")
                )(DbUrl(_))

              val dbImpl: Database = new Database(dbUrl)
              a[DbComponent].inject0.wire
            }
        )
    )
