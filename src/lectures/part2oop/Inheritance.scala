package lectures.part2oop

object Inheritance extends App {

  // single class inheritance
  sealed class Animal {
    protected val creatureType = "wild"
    def eat = println("nomnon "+ creatureType)
  }

  //val animal = new Animal
  //println(animal.creatureType)
  //animal.eat


  class Cat extends Animal{
    def crunch = {
      eat
      println("crunch crunch"+ creatureType)
    }
  }

  val cat = new Cat
  cat.eat
  cat.crunch

  // constructors
  class Person(name: String, age: Int){
    def this(name: String) = this(name,0)
  }

  class Adult(name: String, age: Int, idCard: String) extends Person(name)

  // overriding
  class Dog(override val creatureType: String) extends Animal {
    //    override val creatureType = "domestic" // can override in the body or directly in the constructor arguments
    override def eat = {
      super.eat
      println("I eat cats")
    }

    def eat(food: String) = {
      println(s"I eat $food")
    }

  }

  val dog = new Dog("K9")
  dog.eat
  dog.eat("trash")
  println(dog.creatureType)

  // type substitution (broad: polymorphism)
  val unknownAnimal: Animal = new Dog("K9")
  unknownAnimal.eat

  // overRIDING vs overLOADING

  // super

  // preventing overrides
  // 1 - use final on member
  // 2 - use final on the entire class
  // 3 - seal the class = extend classes in THIS FILE, prevent extension in other files

}
