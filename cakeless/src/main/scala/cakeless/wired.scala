package cakeless

import scala.annotation.StaticAnnotation

/**
  * Used to explicitly indicate the dependency for .wire macro
  * when several instances of the same type are present
  * */
class wired extends StaticAnnotation
