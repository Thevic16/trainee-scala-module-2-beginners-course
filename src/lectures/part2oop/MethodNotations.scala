package lectures.part2oop

object MethodNotations extends App {

  class Person(val name: String, favoriteMovie: String, val age: Int = 0){
    def likes(movie: String): Boolean = movie == favoriteMovie
    def +(person: Person): String = s"${this.name} is hanging out with ${person.name}"

    // Exercise #1
    def +(nickname: String): Person = new Person(s"$name ($nickname)", favoriteMovie, age)
    def unary_! : String = s"$name, what the heck?!"

    // Exercise #2
    def unary_+ : Person = new Person(name, favoriteMovie, age + 1)

    // Exercise #3
    def learns(topic: String): String = s"$name learns $topic"
    def learnsScala : String = learns("Scala")

    def isAlive: Boolean = true
    def apply(): String = s"Hi, my name is $name and I like $favoriteMovie"
    // Exercise #4
    def apply(times: Int): String = s"$name watched $favoriteMovie 2 times"

  }

  val mary = new Person("Mary", "Inception")
  println(mary.likes("Inception"))
  println(mary likes "Inception") // equivalent
  // infix notation = operator notation (syntactic sugar)

  // "operators" in Scala
  val tom = new Person("Tom", "Fight Club")
  println(mary + tom)
  println(mary.+(tom))

  println(1 + 2)
  println(1.+(2))

  // ALL OPERATORS ARE METHODS.
  // Akka actors have ! ?

  // prefix notation
  val n = 2
  println(-n)  // equivalent with 1.unary_-
  println (n.unary_-)

  println(+n)
  println (n.unary_+)

  println(~n)
  println (n.unary_~)

  // unary_ prefix only works with - + ~ !

  println(!mary)
  println(mary.unary_!)

  // postfix notation
  println(mary.isAlive)
  println(mary isAlive) // only available with the scala.language.postfixOps import - discouraged

  // apply
  println(mary.apply())
  println(mary()) // equivalent

  /*
  1.  Overload the + operator
      mary + "the rockstar" => new person "Mary (the rockstar)"
  2.  Add an age to the Person class
      Add a unary + operator => new person with the age + 1
      +mary => mary with the age incrementer
  3.  Add a "learns" method in the Person class => "Mary learns Scala"
      Add a learnsScala method, calls learns method with "Scala".
      Use it in postfix notation.
  4.  Overload the apply method
      mary.apply(2) => "Mary watched Inception 2 times"
 */

  println((mary + "the Rockstar").apply())
  println(mary.age)
  println((+mary).age)
  println(mary learnsScala)
  println(mary(10))

}
