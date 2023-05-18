package functors

import scala.util.Try

object Functors {

  val aModifiedList = List(1,2,3).map(_ + 1)

  import cats.Functor
  import cats.instances.list._
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1,2,3))(_ + 1)

  import cats.instances.option._
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Some(2))(_ + 1)

  import cats.instances.try_._

  val anIncrementedTry = Functor[Try].map(Try(1))(_ + 1)

  /**
   * Very useful for generalizing APIs
   */
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  //generalized one, but still Scala doesn't know that this F has access to map method
  def do10xNotEnoughGeneric[F[_]](container: F[Int]) = ???

  //But with Functor we can do it!!
  import cats.syntax.functor._
  def do10x[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  //TODO: Exercise 1 - define your own functor for a bynary tree

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
      case _ => throw new RuntimeException()
    }
  }
  trait Tree[+T]

  object Tree {
    //Smart constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T,left: Tree[T], right: Tree[T]) extends Tree[T]

  val tree: Tree[Int] = Tree.branch(10, Tree.branch(10, Tree.leaf(10), Tree.leaf(20)), Tree.branch(10, Tree.leaf(10), Tree.leaf(20)))
  val incrementedTree: Tree[Int] = tree.map(_ + 1)
  def main(args: Array[String]): Unit = {

    println(do10x(List(1,2,3)))
    println(do10x(Option(1)))
    println(do10x(Try(1)))
    /* The compiler doesn't complain becuase the Object (Smart constructors) alway return a generic Tree[T] both for Leaf and Branch*/
    println(do10x(Tree.branch(10, Tree.leaf(10), Tree.leaf(10))))
  }
}
