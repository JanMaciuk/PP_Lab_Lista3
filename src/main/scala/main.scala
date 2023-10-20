import scala.annotation.tailrec


@tailrec
def getLastElement(lista: List[Int]): Option[Int] = lista match {
  case Nil => None
  case h :: Nil => Some(h)
  case h :: t => getLastElement(t)
  // przechodzę dalej dopóki head nie będzie Nil (czyli jestem na ostatnim elemencie)
}

@tailrec
def getTwoLastElements(lista: List[Int], previousElement: Integer): Option[List[Int]] = lista match {
  case Nil => None // Pusta lista
  case h :: Nil => if previousElement!=null then Some(List(previousElement, h)) else None // lista jednoelementowa
  case h :: t => getTwoLastElements(t, h)
  // Analogiczne do pierwszej, ale przenoszę przedostatni element do następnego wywołania
}

def listLength[A](list: List[A]): Int = list match{
  case Nil => 0
  case head :: tail => 1 + listLength(tail)
}

@tailrec
def reverseList[A](list: List[A], newList: List[A]): List[A] = list match{
  case Nil => newList
  case head :: tail => reverseList(tail, head :: newList)
  // Z każdym wykonaniem dopisuje pierwszy element do nowej listy
}

@tailrec
def palindrome[A](list: List[A]): Boolean = list match {
  case Nil => true // Jeżeli usunęliśmy wszystkie elementy przy sprawdzaniu to mamy palindrom
  case head :: Nil => true // Ostatni element, jeżeli tu dotarliśmy to mamy palindrom
  case head :: tail => if (head == tail.last) palindrome(tail.dropRight(1)) else false
  // Jeżeli pierwszy element jest równy ostatniemu to wykonaj ponownie bez pierwszego i ostatniego elementu
}

def evenIndexOnly[A](list: List[A], evenIndex: Boolean): List[A] = list match {
  case Nil => Nil
  case head :: tail => if evenIndex then head :: evenIndexOnly(tail, !evenIndex) else evenIndexOnly(tail, !evenIndex)
  //jeżeli jestem na parzystym indeksie to biorę element, jeżeli nie to pomijam
}



@main
def main(): Unit = {
  println("Hello world!")
  println(getLastElement(List(1, 2, 3, 4, 5)))                        // Zadanie 1
  println(getTwoLastElements(List(1, 2, 3, 4, 5), null)) // Zadanie 2
  println(listLength(List(1, 2, 3, 4, 5)))                            // Zadanie 3
  println(reverseList(List(1, 2, 3, 4, 5), List()))                   // Zadanie 4
  println(palindrome(List(1, 2, 3, 4, 5)))                            // Zadanie 5
  //TODO: Zadanie 6
  println(evenIndexOnly(List(1, 2, 3, 4, 5), true))          // Zadanie 7


}