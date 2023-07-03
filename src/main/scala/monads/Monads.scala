package monads

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Monads {

  val numbersList: List[Int] = List(1,2,3)
  val charsList: List[Char] = List('a','b','c')

  //TODO: Exercise 1 --> Combine chars and list in all the combinations
  val charsAndNumbersCombined: List[(Int, Char)]  = for {
    number <- numbersList
    char <- charsList
  } yield (number, char)

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A,B](value: M[A])(func: A => M[B]): M[B]
    // Since Monads has map method, Monads extends functors, so Monads are Functors.
    def map[A,B](value: M[A])(func: A => B): M[B] = flatMap(value)(x => pure(func(x)))
  }

  // Cats Monad
  import cats.Monad
  import cats.syntax.flatMap._ // this allows us to use for comprehensions
  import cats.instances.option._
  import cats.syntax.functor._
  import cats.syntax.applicative._

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if(x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  /**
   * Wrapping a pure value into a monadic value
   */
  val aList = listMonad.pure(1)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1))

  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  /**
   * If we want to do the same thing but with an Option monad we have no way but to duplicate it
   */
  def getPairsList(numbers: Option[Int], chars: Option[Char]): Option[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  /**
   * But with a Monad we can have the same implementation for any type of monad with a Monad implicit instance
   */
  def getPairsListGeneric[M[_]: Monad, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    number <- numbers
    char <- chars
  } yield (number, char)


  import cats.syntax.applicative._

  val oneOptionWithExtension = 1.pure[Option] // implicit Monad[Option] will be used
  val oneListWithExtension = 1.pure[List]

  /**
   * Since List, Option, Try, Future etc.. are Monads, you can achieve the same result (combining all the values inside the Monad)
   * with the same exact piece of code.
   */

  import cats.Monad
  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  import cats.instances.list._

  val monadList = Monad[List]
  val aSimpleList = monadList.pure(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]
  val anEither = loadingMonad.pure(42)
  val aChangedLoading = loadingMonad.flatMap(anEither)(item => if(item % 2 == 0 ) Right(1) else Left("There is an error"))

  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "OK"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] = if (orderStatus.status == "OK") Right("Chignolo po") else Left("Unable to fetch location")

  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(configs: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  case class VeryGenericException() extends Exception

  /**
   * The very cool approach here is that we can define the wrapper as we want, it can be a
   *  Future, a Try or also an Either, and we can build implementations for all this different types
   *  of monads.
   */
  class HttpServiceImplEither extends HttpService[ErrorOr] {
    override def getConnection(configs: Map[String, String]): ErrorOr[Connection] = (config.get("host"), config.get("port")) match {
      case (Some(host), Some(port)) => Right(Connection(host, port))
      case (_, _) => Left(VeryGenericException())
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = {
      payload.length match {
        case size if size < 10 => Right(s"request ($payload) has been accepted")
        case _ => Left(VeryGenericException())
      }
    }
  }

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  /**
   * This will work with whatever HttpService we're going to provide, very cool stuff!!!
   */
  def getResponse[M[_]](httpService: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] = for {
    connection <- httpService.getConnection(config)
    response <- httpService.issueRequest(connection, payload)
  } yield response
  def main(args: Array[String]): Unit = {
    val orderId = 1234
    val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(status => trackLocation(status))

    for {
      status <- getOrderStatus(orderId)
      location <- trackLocation(status)
      _ = println(location)
    } yield location

    // EXERCISE
    val httpService = new HttpServiceImplEither()
    httpService.getConnection(config) match {
      case Left(e) => throw e
      case Right(connection) => {
        println(httpService.issueRequest(connection, "12345"))
      }
    }

    //Improved version
    println(getResponse(httpService, "Payload"))

  }
}