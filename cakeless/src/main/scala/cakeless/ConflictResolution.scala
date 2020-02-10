package cakeless

sealed trait ConflictResolution
object ConflictResolution {
  sealed trait Raise extends ConflictResolution
  case object Raise  extends Raise

  sealed trait Warn extends ConflictResolution
  case object Warn  extends Warn

  /**
    * @note - automatic conflict resolution not implemented yet
    * */
//  sealed trait Auto extends ConflictResolution
//  case object Auto  extends Auto
}
