package evaluation

object Evaluation {

  /**
   * Cats makes the distinction between
   * - Eager evaluation: the value is computed as soon as it is defined
   * - Lazy evaluation: the value is evaluated lazily and every time you need it
   * - Memoized evaluation: the value is evaluated lazily but only the first time you need it
   *
   */

  import cats.Eval

  // this will be evaluated even if we don't use it
  val instantEval: Eval[Int] = Eval.now{
    println("Computing now!")
    1 + 1
  }

  // this will be evaluated  ONLY if we use it
  val alwaysEval: Eval[Int] = Eval.always{
    println("Computing alwaysEval when needed!")
    1 + 1
  }

  val delayedEval: Eval[Int] = Eval.later{
    println("Computing later")
    1 + 1
  }

  val composedEvaluation: Eval[Int] = delayedEval.flatMap(value => instantEval.flatMap(value2 => {
    Eval.later(value + value2)
  }))

  val anotherComposedEvaluation = for{
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2

  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  def reverseList[T](list: List[T]): List[T] =
    if(list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseListWithEval[T](list:List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else defer(reverseListWithEval(list.tail).map(_ :+ list.head))
  def main(args: Array[String]): Unit = {
    println(alwaysEval.value)
    //this will print also "Computing Later"
    println(delayedEval.value)
    //this will NOT print also "Computing Later" since the value is already memoized
    println(delayedEval.value)
    println(s"Composed evaluation: ${composedEvaluation.value}")
    println(s"Composed evaluation: ${composedEvaluation.value}")

    defer(Eval.now({
      println("This should appear only when .value")
      1 + 1
    }))

    println(reverseListWithEval((1 to 10000).toList).value)
  }
}
