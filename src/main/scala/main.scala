import scala.annotation.tailrec


@tailrec
def getLastElement(lista: List[Int]): Option[Int] = lista match {
  case Nil => None
  case h :: Nil => Some(h)
  case h :: t => getLastElement(t)
}


@main
def main(): Unit = {
  println("Hello world!")
  println(getLastElement(List(1, 2, 3, 2)))


}