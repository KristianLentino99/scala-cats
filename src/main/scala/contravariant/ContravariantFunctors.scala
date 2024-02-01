package contravariant

object ContravariantFunctors {

  trait Format[T] { self =>
      def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }
  def format[A](value: A)(implicit f  : Format[A]): String = f.format(value)
  implicit object StringFormat extends Format[String]{
    override def format(value: String): String = s"\"$value\""
  }

  implicit object IntFormat extends Format[Int]{
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean]{
    override def format(value: Boolean): String = if(value) "yes" else "no"
  }

  // problem: if we have Format[MyType], can we have automatically Format[Option[MyType]]? or any other monadic type
  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = f.contramap[Option[T]](_.get) //very bad and minimal implementation, shouldn't use the get method

  def main(args: Array[String]): Unit = {
    println("Hello, ContravariantFunctors!")
    println(format(true))
    println(format(Option(10)))
    println(format(Option(Option(11)))) // since the compiler has access to a Format[Option[Int]], it will use it recursively as many times as needed, so you can wrap the int in how many Option you want/need
  }
}
