package lectures.part2oop

object AnonymousClasses extends App {

  abstract class Animal {
    def eat: Unit
  }

  // anonymous class
  val funnyAnimal: Animal = new Animal {
    override def eat: Unit = println("ahahahahahaah")
  }

  /*
  equivalent with
  class AnonymousClasses$$anon$1 extends Animal {
    override def eat: Unit = println("ahahahahahaah")
  }
  val funnyAnimal: Animal = new AnonymousClasses$$anon$1
 */
  println(funnyAnimal.getClass)

  // apparently we only have one instance of a anonymous class like object (Victor Gomez's comment)
  println(funnyAnimal.eat)

  trait MyAnimal {
    def eat: Unit
  }

  val predator = new MyAnimal {
    override def eat: Unit = println("RAWR!")
  }

  println(predator.eat)


  class Person(name: String) {
    def sayHi: Unit = println(s"Hi, my is $name, how can I help?")
  }

  val jim = new Person("Jim") {
    override def sayHi: Unit = println(s"Hi, my name is Jim, how can I help?")
  }

  println(jim.sayHi)
}
