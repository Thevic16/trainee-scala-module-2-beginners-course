package lectures.part4pm

import exercises.{Cons, Empty, MyList}

object AllThePatterns extends App {
  // 1 - constant
  val x: Any = "Scala"
  val constants = x match {
    case 1 => "a number"
    case "Scala" => "THE Scala"
    case true => "The Truth"
    case AllThePatterns => "A singleton object"
  }

  // 2 - match anything
  // 2.1 wildcard
  val matchAnything = x match {
    case _ => "anything"
  }

  // 2.2 variable
  val matchAVariable = x match {
    case something => s"I've found $something"
  }

  // 3 - tuples
  val aTuple = (1, 2)
  val matchATuple = aTuple match {
    case (1, 1) => "tuple (1, 1)"
    case (something, 2) => s"I've found $something"
  }

  val nestedTuple = (1, (2, 3))
  val matchANestedTuple = nestedTuple match {
    case (_, (2, v)) => s"$v"
  }

  // PMs can be NESTED!

  // 4 - case classes - constructor pattern
  // PMs can be nested with CCs as well
  val aList: MyList[Int] = Cons(1, Cons(2, Empty))
  val matchAList = aList match {
    case Empty => "Empty"
    case Cons(head, Cons(subhead, subtail)) => s"head: $head subhead: $subhead subtail: $subtail"
  }

  println("4 - case classes - constructor pattern \n" + matchAList)

  // 5 - list pattern
  val aStandardList = List(6, 5, 2, 3, 42)
  val standardListMatching = aStandardList match {
    case List(1, _, _ , _) => "List(1, _, _ , _)" // extractor - advanced
    case List(1, _*) => "List(1, _*)" //  list of arbitrary length - advanced
    case 6 :: List(_*) => "6 :: List(_*)" // infix pattern
    case List(5, 2, _) :+ 42 => "List(5, 2, _) :+ 42" // infix pattern
    case _ => "No found"
  }

  println("\n\n5 - list pattern \n" + standardListMatching)

  // 6 - type specifiers
  val unknown: Any = 2
  val unknownMatch = unknown match {
    case list: List[Int] => s"List type $list" // explicit type specifier
    case _ => "unknown"
  }

  // 7 - name binding
  val nameBindingMatch = aList match {
    case nonEmptyList @ Cons(_, _) => s"$nonEmptyList" // name binding => use the name later(here)
    case Cons(1, rest @ Cons(2, _)) => s"$rest" // name binding inside nested patterns
  }

  // 8 - multi-patterns
  val multipattern = aList match {
    case Empty | Cons(0, _) => "yes Sir!" // compound pattern (multi-pattern)
  }

  // 9 - if guards
  val secondElementSpecial = aList match {
    case Cons(_, Cons(specialElement, _)) if specialElement % 2 == 0 => "Yes"
  }

  val anything: Any = ???
  anything match {
    case _: RuntimeException | _: NullPointerException => ""
  }

  // ALL.

  /*
    Question.
   */

  val numbers = List(1, 2, 3)
  val numbersMatch = numbers match {
    case listOfStrings: List[String] => "a list of strings"
    case listOfNumbers: List[Int] => "a list of numbers"
    case _ => ""
  }

  println(numbersMatch)
  // JVM trick question

}
