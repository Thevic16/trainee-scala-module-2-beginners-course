package lectures.part1basics

object Functions extends App {
  def aFunction(a: String, b: Int): String = {
    a + " " + b
  }

  println(aFunction("hello", 3))

  def aParameterlessFunction(): Int = 42

  println(aParameterlessFunction())
  // println(aParameterlessFunction) // only works in Scala 2 - parameterless functions are invoked without parentheses

  def aRepeatedFunction(aString: String, n: Int): String = {
    if (n == 1) aString
    else aString + aRepeatedFunction(aString, n-1)
  }

  println(aRepeatedFunction("hello", 3))

  // WHEN YOU NEED LOOPS, USE RECURSION.

  def aFunctionWithSideEffects(aString: String): Unit = println(aString)

  def aBigFunction(n: Int): Int = {
    def aSmallerFunction(a: Int, b: Int): Int = a + b

    aSmallerFunction(n , n-1)
  }

  /*
  Exercises:
  1.  A greeting function (name, age) => "Hi, my name is $name and I am $age years old."
  2.  Factorial function 1 * 2 * 3 * .. * n
  3.  A Fibonacci function
      f(1) = 1
      f(2) = 1
      f(n) = f(n - 1) + f(n - 2)
  4.  Tests if a number is prime.
 */

  def greeting(name: String, age: Int): String = {
    s"Hi, my name is $name and I am $age years old."
  }

  def factorial(n: Int): Int ={
    if (n == 1) n
    else n * factorial(n-1)
  }

  println(factorial(5))

  /*
  https://www.google.com/search?q=fibonacci&source=lnms&tbm=isch&sa=X&ved=2ahUKEwiRm7nbi4v5AhX3mIQIHXelCWcQ_AUoAXoECAIQAw&biw=1242&bih=574&dpr=1.1#imgrc=St65zpeiadQMZM
   */
  def fibonacci(n: Int): Int = {
    if (n == 1 || n == 2) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }
  println(fibonacci(8))

  def prime(n: Int): Boolean = {
    def condition(t: Int): Boolean = {
      if (t == 1) true
      else if (t == 0) false
      else if (n % t == 0) false
      else condition(t-1)
    }

    condition(n-1)
  }

  println(prime(1))
  println(prime(2))
  println(prime(3))
  println(prime(4))
  println(prime(5))
  println(prime(6))
  println(prime(7))
  println(prime(68))

  println(prime(37))
  println(prime(2003))
  println(prime(37 * 17))

}
