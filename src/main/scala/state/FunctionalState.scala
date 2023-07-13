package state

import cats.Eval
import cats.data.IndexedStateT

object FunctionalState {

  type MyState[S, A] = S => (S, A)
  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value

  //state = "iterative" computations

  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  //pure FP with states
  val firstTransformation: State[Int, String] = State((s: Int) => (s+1, s"Added 1, obtained ${s + 1}"))
  val secondTransformation: State[Int, String] = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap(firstResult => secondTransformation.map(secondResult => (firstResult, secondResult)))

  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  //Why not use just functions and compose them?
  val func1 = (s: Int) => (s +1 , s"Added 1, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5 , s"Multiplied by 5, obtained ${s * 5}")

  val compositeFunc: Int => (String, (Int, String)) = func1.andThen{
    case (newState, firstResult) => (firstResult, func2(newState))
  }

  //TODO - Exercise 1: online store
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State{
    shoppingCart: ShoppingCart => {
      (ShoppingCart(items = item :: shoppingCart.items, total = shoppingCart.total + price), price + shoppingCart.total)
    }
  }

  // TODO - Exercise 2: return a data structures that when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State {initialValue =>
    (initialValue, f(initialValue))
  }
  //returns a state data structure that, when run, will return the value of that state and makes no changes
  def get[A]: State[A, A] = State{ initialValue =>
    (initialValue, initialValue)
  }
  //returns a state data structure that, when run, will return Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State{ _ =>
    (value, ())
  }
  def modify[A](f: A => A): State[A, Unit] = State{ value =>
    (f(value), ())
  }

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)
  def main(args: Array[String]): Unit = {
    println(compositeTransformation2.run(10).value)
    println(compositeFunc(10))
    val cart = ShoppingCart(
      items = List("Caffe", "Mouse", "Caffe machine"),
      total = 130
    )

    println(addToCart("Cat food", 10).run(cart).value)

    println(program.run(10).value)
  }
}
