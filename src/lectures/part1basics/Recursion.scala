package lectures.part1basics

import scala.annotation.tailrec

object Recursion extends App {

  def factorial(n: Int): Int =
    if (n <= 1) 1
    else {
      println("Computing factorial of " + n + " - I first need factorial of " + (n-1))
      val result = n * factorial(n-1)
      println("Computed factorial of " + n)

      result
    }

  // println(factorial(10))
  //println(factorial(5000)) // stack overflow!

  def anotherFactorial(n: Int): BigInt = {
    @tailrec
    def factHelper(x: Int, accumulator: BigInt): BigInt =
      if (x <= 1) accumulator
      else factHelper(x - 1, x * accumulator) // TAIL RECURSION = use recursive call as the LAST expression

    factHelper(n, 1)
  }

  /*
  Breakdown:

  anotherFactorial(10) = factHelper(10, 1)
  = factHelper(9, 10 * 1)
  = factHelper(8, 9 * 10 * 1)
  = factHelper(7, 8 * 9 * 10 * 1)
  = ...
  = factHelper(2, 3 * 4 * ... * 10 * 1)
  = factHelper(1, 1 * 2 * 3 * 4 * ... * 10)
  = 1 * 2 * 3 * 4 * ... * 10
 */

  // println(anotherFactorial(20000))

  // WHEN YOU NEED LOOPS, USE _TAIL_ RECURSION.

  /*
    Exercises:
    1.  Concatenate a string n times
    2.  IsPrime function tail recursive
    3.  Fibonacci function, tail recursive.
   */

  def aRepeatedFunction(aString: String, n: Int): String = {
    @tailrec
    def repeatHelper(aStringAccumulator: String, t: Int): String =
      if (t <= 1) aStringAccumulator
      else repeatHelper(aStringAccumulator+aString, t-1)

    repeatHelper(aString, n)
  }

  println(aRepeatedFunction("hello", 5))

  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeHelper(t: Int): Boolean = {
      if (t <= 1) true
      else if (n % t == 0) false
      else isPrimeHelper(t-1)
    }

    isPrimeHelper(n-1)
  }

  println(isPrime(1))
  println(isPrime(2))
  println(isPrime(3))
  println(isPrime(4))
  println(isPrime(5))
  println(isPrime(6))
  println(isPrime(7))
  println(isPrime(68))

  println(isPrime(37))
  println(isPrime(2003))
  println(isPrime(37 * 17))


  def fibonacci(n: Int): Int ={
    // The amount of accumulator is equal to the amount of the recursive calls
    @tailrec
    def fibonacciHelper(i: Int, last: Int, nextToLast: Int): Int =
      if (i >= n) last + nextToLast
      else fibonacciHelper(i+1, last + nextToLast, last )

    if (n<=2) 1
    else fibonacciHelper(3, 1, 1)
  }

  println(fibonacci(7))
  println(fibonacci(8))

}
