package exercises


abstract class Maybe[+A] {
  def head: A
  def isEmpty: Boolean
  def add[B >: A](element: B): Maybe[B]
  def printElements: String

  // polymorphic call
  override def toString: String = "["+ printElements +"]"

  // Exercise
  // higher-order functions
  def map[B](transformer: A => B): Maybe[B]
  def filter(predicate: A => Boolean): Maybe[A]
  def flatMap[B](transformer: A => Maybe[B]): Maybe[B]
}

case object EmptyOne extends Maybe[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](element:B): Maybe[B] =  One(element)
  override def printElements: String = ""

  // Exercise
  override def map[B](transformer: Nothing => B): Maybe[B] = EmptyOne
  override def filter(predicate: Nothing => Boolean): Maybe[Nothing] = EmptyOne
  override def flatMap[B](transformer: Nothing => Maybe[B]): Maybe[B] = EmptyOne

}

case class One[+A](h: A) extends Maybe[A] {
  override def head: A = h
  override def isEmpty: Boolean = false
  override def add[B >: A](element: B): Maybe[B] =  throw new RuntimeException("You can't add elements to One list")
  override def printElements: String = "" + h


  // Exercise
  override def map[B](transformer: A => B): Maybe[B] = One(transformer(h))


  override def filter(predicate: A => Boolean): Maybe[A]=
    if (predicate(h))  One(h)
    else EmptyOne

  override def flatMap[B](transformer: A => Maybe[B]): Maybe[B] = transformer(h)
}

object MaybeTest extends App {

  val oneInt1 = One[Int](1)
  val empty = Empty

  println(oneInt1.map(_ + 5))
  println(oneInt1.flatMap((x: Int) => One(x * 2)))
  println(oneInt1.filter(_ % 2 == 0))
  println(empty.add(5))

}
