import scala.annotation.tailrec


@tailrec // Zadanie 1
def getLastElement[A](lista: List[A]): Option[A] = lista match {
  case Nil => None
  case h :: Nil => Some(h)
  case h :: t => getLastElement(t)
  // przechodzę dalej dopóki head nie będzie Nil (czyli jestem na ostatnim elemencie)
}

@tailrec // Zadanie 2
def getTwoLastElements[A](lista: List[A], previousElement: A): Option[List[A]] = lista match {
  case Nil => None // Pusta lista
  case h :: Nil => if previousElement!=null then Some(List(previousElement, h)) else None // lista jednoelementowa
  case h :: t => getTwoLastElements(t, h)
  // Analogiczne do pierwszej, ale przenoszę przedostatni element do następnego wywołania
}

//Zadanie 3
def listLength[A](list: List[A]): Int = list match{
  case Nil => 0
  case head :: tail => 1 + listLength(tail)
}

@tailrec // Zadanie 4
def reverseList[A](list: List[A], newList: List[A]): List[A] = list match{
  case Nil => newList
  case head :: tail => reverseList(tail, head :: newList)
  // Z każdym wykonaniem dopisuje pierwszy element do nowej listy
}

@tailrec // Zadanie 5
def palindrome[A](list: List[A]): Boolean = list match {
  case Nil => true // Jeżeli usunęliśmy wszystkie elementy przy sprawdzaniu to mamy palindrom
  case head :: Nil => true // Ostatni element, jeżeli tu dotarliśmy to mamy palindrom
  case head :: tail => if (head == getLastElement(tail).get) palindrome(deleteLastElement(tail)) else false
  // Jeżeli pierwszy element jest równy ostatniemu to wykonaj ponownie bez pierwszego i ostatniego elementu
}

//Funkcja pomocnicza do polindromów, żeby uniknąć używania gotowych metod (overkill)
def deleteLastElement[A](list: List[A]): List[A] = list match {
  case Nil => Nil
  case head :: Nil => Nil
  case head :: tail => head :: deleteLastElement(tail)
  // Przechodzę aż do ostatniego elementu, który pomijam
}

//Zadanie 6
def uniqueElementsOnly[A](list: List[A]): List[A] = list match {
  case Nil => Nil
  case head :: tail => if listContains(tail, head) then uniqueElementsOnly(tail)
  else head :: uniqueElementsOnly(tail)
  // Jeżeli reszta listy zawiera pierwszy element to jest pomijany, jeżeli nie to dodawany do listy zwracanej
  // Nie możemy używać gotowych metod do pracy na listach więc napisałem własne listContains
}
@tailrec
def listContains[A](list: List[A], element: A): Boolean = list match {
  case Nil => false
  case head :: tail => if head == element then true
  else listContains(tail, element)
  // Przechodzę całą listę element po elemencie, sprawdzając czy jest równy szukanemu
}

// Zadanie 7
def evenIndexOnly[A](list: List[A], evenIndex: Boolean): List[A] = list match {
  case Nil => Nil
  case head :: tail => if evenIndex then head :: evenIndexOnly(tail, !evenIndex)
  else evenIndexOnly(tail, !evenIndex)
  //jeżeli jestem na parzystym indeksie to biorę element, jeżeli nie to pomijam
}

@tailrec
def checkPrime(n: Int, i: Int): Boolean = {
  if (n<=1) false         // 1 lub ujemne nie są pierwsze
  else if (n<=3) true     // 2 i 3 są pierwsze
  else if (n%i==0) false  // jeżeli jest podzielna to nie jest pierwsza
  else if (i*i>n) true    // według wikipedi wszystkie dzielniki większe od pierwiastka z n muszą mieć inny już sprawdzony dzielnik
  else checkPrime(n, i+1) // sprawdzam podzielność przez następną liczbę

}



@main
def main(): Unit = {
  println("Hello world!")
  println(getLastElement(List(1, 2, 3, 4, 5)))                        // Zadanie 1
  println(getTwoLastElements(List(1, 2, 3, 4, 5), null)) // Zadanie 2
  println(listLength(List(1, 2, 3, 4, 5)))                            // Zadanie 3
  println(reverseList(List(1, 2, 3, 4, 5), List()))                   // Zadanie 4
  println(palindrome(List(1, 2, 3, 4, 5)))                            // Zadanie 5
  println(uniqueElementsOnly(List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)))     // Zadanie 6
  println(evenIndexOnly(List(1, 2, 3, 4, 5), true))          // Zadanie 7
  println(checkPrime(17, 2))                                           // Zadanie 8

}