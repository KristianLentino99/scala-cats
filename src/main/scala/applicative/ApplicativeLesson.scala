package applicative

object ApplicativeLesson {

  // Applicatives = Functors + the pure method
  // Monads extends Applicative
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2)
  import cats.instances.option._
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2)

  /**
   * Why Are applicatives useful?
   * - Very useful at generalize code like Functors
  */
  import cats.syntax.applicative._
  val aListWithSyntax = 2.pure[List] // List(2)
  val anOptionWithSyntax = 2.pure[Option] //Some(2)

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(2)
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1)
  val validatedApplicative = Applicative[ErrorsOr]

  //TODO: Exercise
  def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ??? //already implemented in cats
  def productWithApplicatives[F[_], A, B](fa: F[A], fb: F[B])(implicit applicative: Applicative[F]): F[(A,B)] = {
    val functionWrapper: F[B => (A,B)] = applicative.map(fa)(a => (b: B) => (a,b))
    applicative.ap(functionWrapper)(fb)
  }

  def main(args: Array[String]): Unit = {

    println(productWithApplicatives(List(1,2), List("a","b")))
  }
}
