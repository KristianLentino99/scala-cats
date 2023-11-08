package errors

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object ErrorsHandling {

  trait MyApplicativeError[M[_], E ] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](fa: M[A])(f: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends Monad[M] with MyApplicativeError[M, E] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val mondErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
  val success: ErrorOr[Int] = mondErrorEither.pure(22)
  val error: ErrorOr[Int] = mondErrorEither.raiseError[Int]("Invalid number") // Left("Invalid number")
  val handledError: ErrorOr[Int] =  mondErrorEither.handleError(error){
    case "Invalid number" => 0
    case _ => -1
  }
  mondErrorEither.ensure(success)("Number must be greater than 10")(_ > 10)

  import cats.instances.try_._ // Implicits MonadError[Try]
  val exception = new RuntimeException("Really bad")
  val pureException = MonadError[Try, Throwable].raiseError(exception)
  import cats.instances.future._
  implicit val ex: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future,Throwable].raiseError(exception) //Future which will complete with Failure(exception)

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  val extendedSuccess = 42.pure[ErrorsOr]
  val extendedError: ErrorsOr[Int] = List("Something").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }
  import cats.syntax.monadError._
  val testedSuccess = success.ensure("Something not working")(_ > 100)
  def main(args: Array[String]): Unit = {
    println(error)
    println(extendedError)
  }
}
