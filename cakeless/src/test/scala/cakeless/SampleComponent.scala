package cakeless

trait SampleComponent {
  def foo: SampleDep
}

case class SampleDep(foo: Int)
