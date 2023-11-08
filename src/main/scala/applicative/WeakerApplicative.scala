package applicative

import cats.{Functor, Semigroupal}

/**
 * Weaker Applicatives extends Functor, Semigroupal and has ap method
 */
object WeakerApplicative {

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W]{
    def ap[A, B](wf: W[A => B])(wa: W[A]): W[B] = ???

    def mapN [A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2) // W[(A,B)]
      map(tupleWrapper) { case (a, b) => f(a, b) }
    }
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }
  }
  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](value : A): W[A]
  }
  def main(args: Array[String]): Unit = {

  }

  import cats.Apply
  import cats.instances.option._
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2))

  import cats.syntax.apply._
  val aTupleOfOptions = (Option(1), Option(2), Option(2))
  val optionTupled: Option[(Int, Int, Int)] = aTupleOfOptions.tupled
  val sumOption: Option[Int] = aTupleOfOptions.mapN((a, b, c) => a + b + c)
}
