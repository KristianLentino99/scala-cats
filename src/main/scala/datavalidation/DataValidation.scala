package datavalidation



object DataValidation {

  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(10)
  val anInValidValue: Validated[String, Int] = Validated.invalid("Not working")
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "value too small")

  // TODO: use Either
  def isPrime(n: Int): Boolean = (n > 1) && !(2 until n).exists(x => n % x == 0)
  /**
   * Number must be a prime
   * Number must be non-negative
   * Number <= 100
   * Number must be even
   */
  def testNumber(n: Int): Either[List[String], Int] =  {
    val primeCondition = if(isPrime(n)) List.empty[String] else List("Not prime number")
    val negativeCondition = if(n > 0) List.empty[String] else List("Not positive number")
    val smallerThan100Condition = if(n <= 100) List.empty[String] else List("Value too large")
    val evenCondition = if(n % 2 == 0) List.empty[String] else List("Value is not even")

    if(primeCondition.isEmpty && negativeCondition.isEmpty && smallerThan100Condition.isEmpty && evenCondition.isEmpty) Right(n)
    else Left(primeCondition ++ negativeCondition ++ smallerThan100Condition ++ evenCondition)
  }


  // With Validated approach
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(isPrime(n), n, List("Not prime number"))
      .combine(Validated.cond(n > 0, n, List("Not positive number")))
      .combine(Validated.cond(n <= 100, n, List("Value too large")))
      .combine(Validated.cond(n % 2 == 0, n, List("Value is not even")))


  //chain validation
  aValidValue.andThen(_ => anInValidValue)

  //ensure will check the value against a predicate and return an Invalid if the predicate returns false
  aValidValue.ensure(List("Something is wrong"))(_ % 2 == 0)
  //transform values
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length) // transform the error on the left side
  aValidValue.bimap(_.length, _ + 1) // transform both sides
  //interopearate with std lib
  val anEitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val anOptionToValidated: Validated[List[String], Int] = Validated.fromOption(Some(42), List("Nothing here"))

  //TODO: Exercise form validation
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    def getValueFromForm(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"$fieldName must be specified"))

    def notBlank(value: String,fieldName: String): FormValidation[String] = Validated.cond(!value.isBlank, value, List(s"$fieldName must not be blank"))

    def validateEmail(email: String): FormValidation[String] = Validated.cond(email.contains("@"), email, List("Email must contain @"))

    def validatePassword(password: String): FormValidation[String] = Validated.cond(password.length >= 10, password, List("Password must be at least 10 characters long"))
    /*
    * fields are
    * - name
    * - email
    * - password
    *
    * rules are:
    * - name, email,password  must not be empty
    * - email must contain @
    * - password must have >= 10 characters
     */
    def validateForm(form: Map[String,String]): FormValidation[String] = {
      val (name, email, password) = (getValueFromForm(form, "name").andThen(notBlank(_,"name")), getValueFromForm(form, "email").andThen(notBlank(_,"email")).andThen(validateEmail), getValueFromForm(form, "password").andThen(notBlank(_,"password")).andThen(validatePassword))

      name.combine(email).combine(password).map(_ => "User created")
    }
  }

  def main(args: Array[String]): Unit = {
    import datavalidation.DataValidation.FormValidation.validateForm
    println(
      validateForm(
        Map(
          "name" -> "",
          "email" -> "kristianlentinogmail.com",
          "password" -> "Passwordda10caratteri"
        )
      )
    )
  }
}
