package monads

object Monads {

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
      case Left(_) => VeryGenericException()
      case Right(connection) => {
        println(httpService.issueRequest(connection, "12345"))
      }
    }

    //Improved version
    println(getResponse(httpService, "Payload"))

  }
}