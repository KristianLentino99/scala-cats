package monads

import cats.{Applicative, Apply}

object WeakerMonads {

  trait MyFlatMap[M[_]] extends Apply[M]{
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](w: M[A => B])(wa: M[A]): M[B] =
      flatMap(w)(wf => map(wa)(a => wf(a)))


  }
  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M]{
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def getPairs[M[_]: FlatMap](numbers: M[Int], chars: M[Char]): M[(Int, Char)] =
    for {
      number <- numbers
      char <- chars
    } yield (number, char)
  def main(args: Array[String]): Unit = {

  }

}
