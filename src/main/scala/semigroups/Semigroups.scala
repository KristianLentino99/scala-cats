package semigroups

object Semigroups {

  // Semigroups combine elements of the same type.
  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46)

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("Hello ", "World")
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  def reduceGeneric[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce((left, right) => left |+| right)

  // Exercise 1: support a new type
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (expense1, expense2) =>
    Expense(Math.max(expense1.id, expense2.id), expense1.amount + expense2.amount)
  }
  def main(args: Array[String]): Unit = {
      val numbers = (1 to 10).toList

    import cats.instances.option._
    val numberOptions: List[Option[Int]] = numbers.map(Option(_))
    println(reduceGeneric(numberOptions))

    val aListOfExpenses = List(
      Expense(1, 200),
      Expense(2, 100),
      Expense(4, 50)
    )

    println(reduceGeneric(aListOfExpenses))

    val combinedIntsWithExpressionSyntax = 2 |+| 3
    val combinedExpensesWithExpressionSyntax = Expense(1, 200) |+| Expense(2, 100)
  }
}
