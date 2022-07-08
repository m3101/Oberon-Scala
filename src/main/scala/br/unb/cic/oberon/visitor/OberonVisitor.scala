package br.unb.cic.oberon.visitor

import br.unb.cic.oberon.ast._

/**
 * The abstract definition of an Oberon Visitor.
 * Note: here we are using a hybrid approach for
 * implementing visitors. Although it resembles
 * the OO flavor, we relies on pattern matching
 * to deal with the specificities of all concrete
 * elements of the Oberon language.
 *
 * @author rbonifacio
 * @author m3101
 */
trait OberonVisitor[+T] {
  def visit[V<:Visitable](something: V) : T = ???
}
/**
  * The definition was later rewritten using type
  * constructors to fit a more functional approach
  * and to facilitate pattern matching and reduce
  * boilerplate code repetitions.
  * @author m3101
  */
trait Visitable {
  def accept[T](v:OberonVisitor[T]):T = v.visit(this)
}