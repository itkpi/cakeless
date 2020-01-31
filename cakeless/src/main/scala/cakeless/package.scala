import zio._
import cakeless.internal.{EnvProvider, InjectionMagnet, ZEnvExcluder}
import scala.language.experimental.macros

package object cakeless {
  def injectPrimary[R, E, A](instance: InjectionMagnet[R, E, A]): ZIO[instance.RR[R], E, A] = ???
  def inject[R, E, A](instance: InjectionMagnet[R, E, A])(constructor: Int): ZIO[instance.RR[R], E, A] = ???
}
