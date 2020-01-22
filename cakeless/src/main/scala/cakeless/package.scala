import zio._
import cakeless.internal.{EnvProvider, InjectionMagnet}
import scala.language.experimental.macros

package object cakeless {
  def injectPrimary[R, E, A](instance: InjectionMagnet[R, E, A]): ZIO[Nothing, E, A] = macro EnvProvider.injectPrimaryImpl[R, E, A]
  def inject[R, E, A](instance: InjectionMagnet[R, E, A])(constructor: Int): ZIO[_, E, A] = macro EnvProvider.injectImpl[R, E, A]
}
