package cakeless

package object ioc {
  type Module[+E, +A]   = ZModule[Any, E, A]
  type UModule[+A]      = Module[Nothing, A]
  type URModule[-R, +A] = ZModule[R, Nothing, A]
  type RModule[+A]      = ZModule[Any, Throwable, A]

  def of[M <: ZModuleDecl[_, _, _]](decl: M): ModuleBuilder0[decl.R, decl.E, decl.Wired] =
    new ModuleBuilder0[decl.R, decl.E, decl.Wired](decl.asInstanceOf[ZModule[decl.R, decl.E, decl.Wired]])
}
