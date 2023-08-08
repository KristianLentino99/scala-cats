package semigroupals

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal
  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(Some(123), Some("hello world")) // Some((123, "hello world"))
  val aNoneOption = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._
  implicit val ec: scala.concurrent.ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future(("the meaning of life", 42))

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b")) // List((1, "a"), (1, "b"), (2, "a"), (2, "b"))

  import cats.Monad

  // MONADS EXTENDS SEMIGROUPAL
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A,B)] = monad.flatMap(fa)(a => monad.map(fb)(b => (a,b)))

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr]
  val invalidSemigroupal = validatedSemigroupal.product(
    Validated.invalid(List("something is wrong", "something else is wrong")),
    Validated.invalid(List("A lot of things wrong :D"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]

  val invalidEither = eitherSemigroupal.product(
    Left(List("something is wrong", "something else is wrong")),
    Left(List("A lot of things wrong :D"))
  )
  val rightEither = eitherSemigroupal.product(
    Right(456),
    Right(123)
  )
  def main(args: Array[String]): Unit = {

    println(productWithMonads[List, String, Int](List("Ciaooo"), List(12,3)))

    println(invalidSemigroupal)

    /**
     * When you use the product method on two Left values, it will short-circuit and only combine the errors from the first Left, ignoring the second one.
     * This is because Left represents the "error" side of the Either type.
     */
    println(invalidEither)
    println(rightEither)
  }
}
