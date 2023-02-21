package lectures.part2oop



object OOBasics extends App {
  // constructor
  class Person(name: String, val age: Int = 0){
    // body
    val x = 2

    println(1 + 3)

    // method
    def greet(name: String): Unit = println(s"${this.name} says: Hi, $name")

    // overloading
    def greet(): Unit = println(s"Hi, I am $name")

    // multiple constructors
    def this(name: String) = this(name, 0)
    def this() = this("John Doe")

  }

  val person = new Person("John", 26)
  println(person.x)
  person.greet("Daniel")
  person.greet()


  /*
  Novel and a Writer
  Writer: first name, surname, year
    - method fullname
  Novel: name, year of release, author
  - authorAge
  - isWrittenBy(author)
  - copy (new year of release) = new instance of Novel
 */

  class Writer(firstName: String, surname: String, val year: Int){
    def fullName = s"$firstName $surname"
  }

  class Novel(name: String, yearOfRelease: Int, author: Writer){
    def authorAge: Int = yearOfRelease - author.year

    def isWrittenBy(author: Writer): Boolean = author == this.author

    def copy(newYear: Int) = new Novel(name, newYear, author)
  }

  /*
  Counter class
    - receives an int value
    - method current count
    - method to increment/decrement => new Counter
    - overload inc/dec to receive an amount
 */

  class Counter(val count: Int = 0){
    def currentCount: Int = count

    def incrementCount: Counter = {
      println("Incrementing")
      new Counter(count + 1)
    } // Immutability

    def decrementCount: Counter = {
      println("decrementing")
      new Counter(count - 1)
    }

    /*
    // Easy implementation
    def IncrementCount(amount: Int): Counter = new Counter(count + amount)

    def DecrementCount(amount: Int): Counter = new Counter(count - amount)
     */

    def incrementCount(amount: Int): Counter ={
      if (amount <= 0) this
      else incrementCount.incrementCount(amount - 1)
    }

    def decrementCount(amount: Int): Counter = {
      if (amount <= 0) this
      else decrementCount.decrementCount(amount - 1)
    }

    def print = println(count)
  }

  // Test exercise
  val author = new Writer("Charles", "Dickens", 1812)
  val imposter = new Writer("Charles", "Dickens", 1812)
  val novel = new Novel("Great Expectations", 1861, author)

  println(novel.authorAge)
  println(novel.isWrittenBy(imposter))

  val counter = new Counter
  counter.incrementCount.print
  counter.incrementCount.incrementCount.incrementCount.print
  val counter2: Counter = counter.incrementCount(10)
  counter2.print
  counter2.decrementCount(5).print
}
// class parameters are NOT FIELDS
