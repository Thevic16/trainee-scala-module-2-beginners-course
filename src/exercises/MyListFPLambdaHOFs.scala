package exercises


abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String

  // polymorphic call
  override def toString: String = "["+ printElements +"]"

  // Exercise
  // higher-order functions
  def map[B](transformer: A => B): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]

  // Auxiliary
  def +[B >: A](anotherList: MyList[B]): MyList[B]

  // Higher order functions exercise
  def foreach(f:A => Unit): Unit
  def zipWith[B, C](list: MyList[B], f:(A,B) => C): MyList[C]
  def fold[B](start:B)(f: (B, A)=> B): B
  def sort(compare:(A, A) => Int): MyList[A]
}

case object Empty extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](element:B): MyList[B] =  Cons(element, Empty)
  override def printElements: String = ""

  // Exercise
  override def map[B](transformer: Nothing => B): MyList[Nothing] = Empty
  override def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty
  override def flatMap[B](transformer: Nothing => MyList[B]): MyList[Nothing] = Empty

  // Auxiliary
  override def +[B >: Nothing](anotherList: MyList[B]): MyList[B] = anotherList

  // Higher order functions exercise
  override def foreach(f: Nothing => Unit): Unit = ()
  override def zipWith[B, C](list: MyList[B], f: (Nothing, B) => C): MyList[C] = {
    if (list.isEmpty) Empty
    else throw new RuntimeException("Lists do not have the same length")
  }

  override def fold[B](start: B)(f: (B, Nothing) => B): B = start

  override def sort(compare: (Nothing, Nothing) => Int): MyList[Nothing] = Empty
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  override def head: A = h
  override def tail: MyList[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](element: B): MyList[B] =  Cons(element, this)
  override def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  // Exercise
  override def map[B](transformer: A => B): MyList[B] = {
     Cons(transformer(h), t.map(transformer))
  }

  override def filter(predicate: A => Boolean): MyList[A]= {
    if (predicate(h))  Cons(h, t.filter(predicate))
    else t.filter(predicate)
  }

  override def flatMap[B](transformer: A => MyList[B]): MyList[B] = {
    transformer(h) + t.flatMap(transformer)
  }

  override def +[B >: A](anotherList: MyList[B]): MyList[B] = {
     Cons[B](h , t + anotherList)
  }

  // Higher order functions exercise
  override def foreach(f: A => Unit): Unit = {
    f(head)
    t.foreach(f)
  }

  override def zipWith[B, C](list: MyList[B], f: (A, B) => C): MyList[C] = {
    if (list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else new Cons[C](f(head,list.head), tail.zipWith(list.tail, f))
  }

  override def fold[B](start: B)(f: (B, A) => B): B = {
    tail.fold(f(start, head))(f)
  }

  override def sort(compare: (A, A) => Int): MyList[A] = {
    def insert(x:A, sortedList: MyList[A]): MyList[A] = {
      if(sortedList.isEmpty) Cons(x, Empty)
      else if (compare(x, sortedList.head) <= 0) Cons(x, sortedList)
      else Cons(sortedList.head, insert(x, sortedList.tail))
    }

    insert(h, tail.sort(compare))
  }

}



object ListTest extends App {
  // Test exercise
  val listOfIntegers: MyList[Int] =  Cons(1,  Cons(2,  Cons(3,   Empty)))
  val listOfIntegersZipWith: MyList[Int] =  Cons(4,  Cons(5,  Cons(6,   Empty)))

  // Testing HOHs
  listOfIntegers.foreach(x => println(x))
  println(listOfIntegers.zipWith(listOfIntegersZipWith, (x: Int, y:Int) => x*y).toString)
  //println(listOfIntegers.fold(0)((x:Int ,y:Int) => x + y))
  println(listOfIntegers.fold(0)(_ + _))
  println(listOfIntegers.sort((x:Int, y:Int) => y - x).toString)
  println(listOfIntegers.sort((x:Int, y:Int) => x - y).toString)

  // MapFlatmapFilterFor exercise #1
  val listOfString: MyList[String] = Cons("a",  Cons("b",  Cons("c",   Empty)))
  val listOfColor: MyList[String] = Cons("black",  Cons("white",  Cons("blue",   Empty)))

  // Transform into Map, Flatmap and Filter inside
  val forComprehension: MyList[String] = for {
    n <- listOfIntegers
    c <- listOfString
    color <- listOfColor
  } yield s"$c$n-$color"
  println(forComprehension)

}
