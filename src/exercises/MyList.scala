package exercises
/*
/*
  Exercises:
  1.  Generic trait MyPredicate[-T] with a little method test(T) => Boolean
  2.  Generic trait MyTransformer[-A, B] with a method transform(A) => B
  3.  MyList:
      - map(transformer) => MyList
      - filter(predicate) => MyList
      - flatMap(transformer from A to MyList[B]) => MyList[B]
      class EvenPredicate extends MyPredicate[Int]
      class StringToIntTransformer extends MyTransformer[String, Int]
      [1,2,3].map(n * 2) = [2,4,6]
      [1,2,3,4].filter(n % 2) = [2,4]
      [1,2,3].flatMap(n => [n, n+1]) => [1,2,2,3,3,4]
 */


trait MyPredicate[-T]{
  def test(elem: T): Boolean
}

trait MyTransformer[-A, B]{
  def transform(elem: A): B
}

object HelperMapFilter extends MyPredicate[Int] with MyTransformer[Int, Int]{
  def test(elem: Int): Boolean = {
    if (elem % 2 == 0) true
    else false
  }

  def transform(elem: Int): Int = elem*2
}

object HelperFlatMap extends MyTransformer[Int, MyList[Int]]{
  def transform(elem: Int): MyList[Int] = new Cons[Int](elem, new Cons[Int](elem+1, Empty))
}

abstract class MyList[+A] {

  /*
   head = first element of  the  list
   tail = remainder of the list
   isEmpty = is this list empty
   add(int) => new list with this element added
   toString => a string representation of the list
 */

  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String

  // polymorphic call
  override def toString: String = "["+ printElements +"]"

  // Exercise
  // higher-order functions
  def map[B](transformer: MyTransformer[A, B]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]

  // Auxiliary
  def +[B >: A](anotherList: MyList[B]): MyList[B]
}

case object Empty extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](element:B): MyList[B] = new Cons(element, Empty)
  override def printElements: String = ""

  // Exercise
  override def map[B](transformer: MyTransformer[Nothing, B]): MyList[Nothing] = Empty
  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty
  override def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[Nothing] = Empty

  // Auxiliary
  override def +[B >: Nothing](anotherList: MyList[B]): MyList[B] = anotherList
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  override def head: A = h
  override def tail: MyList[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](element: B): MyList[B] = new Cons(element, this)
  override def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  // Exercise
  override def map[B](transformer: MyTransformer[A, B]): MyList[B] = {
    new Cons(transformer.transform(h), t.map(transformer))
  }

  override def filter(predicate: MyPredicate[A]): MyList[A]= {
    if (predicate.test(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)
  }

  override def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] = {
    transformer.transform(h) + t.flatMap(transformer)
  }

  override def +[B >: A](anotherList: MyList[B]): MyList[B] = {
    new Cons[B](h , t + anotherList)
  }
}



object ListTest extends App {
  /*
  val list = new Cons(1, new Cons(2, new Cons(3, Empty)))
  println(list.tail.head)
  println(list.add(4).head)
  println(list.isEmpty)
  println(list.toString)
   */

  /*
  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings: MyList[String] = new Cons("Hello", new Cons("Scala", Empty))
  val anotherListOfIntegers: MyList[Int] = Empty

  val fakeListOfIntegers = anotherListOfIntegers.add("Hola")
  val ListOfAny = fakeListOfIntegers.add(2)

  println(listOfIntegers.toString)
  println(listOfStrings.toString)
  println(anotherListOfIntegers.toString)
  println(fakeListOfIntegers.toString)
  println(ListOfAny.getClass)
   */

  // Test exercise
  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, new Cons(4, Empty))))
  val listOfIntegersMap = listOfIntegers.map(HelperMapFilter)
  println(listOfIntegersMap.toString)

  val listOfIntegersFilter = listOfIntegers.filter(HelperMapFilter)
  println(listOfIntegersFilter.toString)

  val listOfIntegersFlatMap = listOfIntegers.flatMap(HelperFlatMap)
  println(listOfIntegersFlatMap.toString)

  // Testing Case clases
  val listOfIntegersClone: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, new Cons(4, Empty))))
  println(listOfIntegers == listOfIntegersClone)
}


 */