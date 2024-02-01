package folding

import cats.kernel.Monoid

object Folding {

  object ListExercises {
    def map[A,B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B])((element, currList) => f(element) :: currList )
    def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List.empty[B])((currList, element) => currList.foldRight(f(element))(_ :: _))
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldRight(List.empty[A])((element, acc) => {
        if (predicate(element)) acc :+ element
        else acc
    })

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldLeft(monoid.empty)(monoid.combine)
  }
    def main(args: Array[String]): Unit = {
        import ListExercises._
        val list = List(1, 2, 3, 4, 5)
        println(flatMap(list)(x => List(x, x + 1)))
        import cats.instances.int._
        println(combineAll(list))
    }
}
