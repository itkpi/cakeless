package com.examples

import java.nio.file.Path
import supertagged.TaggedType

object types {
  object ConfigPath extends TaggedType[Path]
  type ConfigPath = ConfigPath.Type

  object Props extends TaggedType[Map[String, String]]
  type Props = Props.Type

  object Token extends TaggedType[String]
  type Token = Token.Type

  object Username extends TaggedType[String]
  type Username = Username.Type

  object Password extends TaggedType[String]
  type Password = Password.Type

  object DbUrl extends TaggedType[String]
  type DbUrl = DbUrl.Type
}
