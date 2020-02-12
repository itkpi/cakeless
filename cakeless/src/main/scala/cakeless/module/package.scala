package cakeless

import zio.Managed

package object module {
  type Module[+E, +A]   = ZModule[Any, E, A]
  type UModule[+A]      = Module[Nothing, A]
  type URModule[-R, +A] = ZModule[R, Nothing, A]
  type RModule[+A]      = ZModule[Any, Throwable, A]

  def of[M <: ModuleDecl[_, _]](decl: M)(environment: Managed[decl.E, decl.Wired]): ModuleBuilder[decl.E, decl.Wired] =
    new ModuleBuilder(environment)
}
