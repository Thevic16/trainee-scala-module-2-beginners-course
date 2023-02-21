package lectures.part3fb

import scala.annotation.tailrec

object TuplesAndMaps extends App {
  // tuples = finite ordered "lists"
  val aTuple = (2, "hello Scala") // Tuple2[Int, String] = (Int, String)

  println(aTuple._1) //2
  println(aTuple.copy(_2 = "goodbye Java"))
  println(aTuple.swap) // ("hello, Scala", 2)

  val aTuple2 = (2, "hello Scala", 2.36, true, 'a')
  //println(aTuple2.swap) // For this is not available

  // Maps - Key -> values
  val aMap: Map[String, Int] = Map()

  val phonebook = Map(("Jim", 555), "Daniel" -> 789, ("JIM", 9000)).withDefaultValue(-1)
  // a -> b is sugar for (a, b)
  println(phonebook)

  // map ops
  println(phonebook.contains("Jim"))
  println(phonebook("Mary"))

  // add a pairing
  val newPairing = "Mary" -> 678
  val newPhonebook = phonebook + newPairing
  println(newPhonebook)

  // functionals on maps
  // map, flatMap, filter
  println(newPhonebook.map(pair => pair._1.toLowerCase -> pair._2))

  // filterKeys
  println(phonebook.filterKeys(x => x.startsWith("J")))

  // mapValues
  println(phonebook.mapValues(number => "809-" + number))

  // conversions to other collections
  println(phonebook.toList)
  println(List(("Daniel", 555)).toMap)

  val names = List("Bob", "James", "Angela", "Mary", "Daniel", "Jim")
  println(names.groupBy(name => name.charAt(0)))
  println(names.groupBy(name => name.charAt(0) == 'J'))

  /*
    1.  What would happen if I had two original entries "Jim" -> 555 and "JIM" -> 900
        !!! careful with mapping keys.
    2.  Overly simplified social network based on maps
        Person = String
        - add a person to the network
        - remove
        - friend (mutual)
        - unfriend
        - number of friends of a person
        - person with most friends
        - how many people have NO friends
        - if there is a social connection between two people (direct or not)
   */
  // 2.
  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
    network + (person -> Set())

  def friend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsPersonA: Set[String] = network(a)
    val friendsPersonB: Set[String] = network(b)
    network + (a -> (friendsPersonA + b)) + (b -> (friendsPersonB + a))
  }

  def unfriend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsPersonA: Set[String] = network(a)
    val friendsPersonB: Set[String] = network(b)
    network + (a -> (friendsPersonA - b)) + (b -> (friendsPersonB - a))
  }


  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {

    @tailrec
    def auxRemove(network: Map[String, Set[String]], friendsPerson: Set[String]): Map[String, Set[String]] =
      if (friendsPerson.isEmpty) network
      else auxRemove(unfriend(network, person, friendsPerson.head), friendsPerson.tail)

     auxRemove(network, network(person)) - person
  }

  def nFriend(network: Map[String, Set[String]], person: String): Int =
    if(network.contains(person)) network(person).size
    else 0

  def mostFriend(network: Map[String, Set[String]]): String =
    network.maxBy(pair => pair._2.size)._1

  def nPeopleWithNoFriends(network: Map[String, Set[String]]): Int = {

    @tailrec
    def nPeopleWithNoFriendsAux(network: Map[String, Set[String]], count: Int): Int =
      if (network.isEmpty) count
      else nPeopleWithNoFriendsAux(network - network.head._1, if (network.head._2.isEmpty) count + 1 else count)

    nPeopleWithNoFriendsAux(network, 0)
  }

  /*
  def socialConnection(network: Map[String, Set[String]], a: String, b: String): Boolean = {
    @tailrec
    def bfs(target: String, consideredPeople: Set[String], discoveredPeople: Set[String]): Boolean = {
      if (discoveredPeople.isEmpty) false
      else if (discoveredPeople.head == target) true
      else if (consideredPeople.contains(discoveredPeople.head)) bfs(target, consideredPeople, discoveredPeople.tail)
      else bfs(target, consideredPeople + discoveredPeople.head, discoveredPeople.tail ++ network(discoveredPeople.head))
    }

    bfs(b, Set(), network(a) + a)
  }
   */

  def socialConnection(network: Map[String, Set[String]], a: String, b: String): Boolean = {
    @tailrec
    def bfs(target: String, consideredPeople: Set[String], discoveredPeople: Set[String]): Boolean = {
      if (discoveredPeople.isEmpty) false
      else {
        val person = discoveredPeople.head
        if (person == target) true
        else if (consideredPeople.contains(person)) bfs(target, consideredPeople, discoveredPeople.tail)
        else bfs(target, consideredPeople + person, discoveredPeople.tail ++ network(person))
      }
    }

    bfs(b, Set(), network(a) + a)
  }


  // Test exercise 2.
  println("\n\n\n TEST SOCIAL NETWORK EXERCISE \n\n\n")
  val person1 = "Bob"
  val person2 = "Mary"
  val person3 = "Victor"
  val person4 = "Gonzalo"

  val emptyNetwork: Map[String, Set[String]] = Map()
  val network0 = add(emptyNetwork, person1)
  val network1 = add(network0, person2)
  val network2 = friend(network1, person1, person2)
  val network3 = unfriend(network2, person1, person2)
  val network4 = add(network3, person3)
  val network5 = add(network4, person4)
  val network6 = friend(network5, person1, person2)
  val network7 = friend(network6, person4, person1)
  val network8 = friend(network7, person4, person2)
  val network9 = friend(network8, person4, person3)
  println(nFriend(network9, person4))
  println(mostFriend(network9))
  val network10 = remove(network9, person4)
  //println(network10)
  println(nPeopleWithNoFriends(network10))
  val network11 = add(network10, person4)
  //println(network11)
  println(socialConnection(network11, person3, person4))
  println(socialConnection(network11, person1, person2))
  val network12 = friend(network11, person1, person3)
  println(network12)
  println(socialConnection(network11, person2, person3))

}
