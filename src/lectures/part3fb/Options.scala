package lectures.part3fb

import scala.util.Random

object Options extends App {

  val myFirstOption: Option[Int] = Some(4)
  val noOption: Option[Int] = None

  println(myFirstOption)

  // WORK with unsafe APIs
  def unsafeMethod(): String = null
  //val result = Some(null) // WRONG
  val result = Option(unsafeMethod()) // Some or None
  println(result)

  // chained methods
  def backupMethod(): String = "A valid result"
  val chainedResult = Option(unsafeMethod()).orElse(Option(backupMethod()))
  println(chainedResult)

  // DESIGN unsafe APIs
  def betterUnsafeMethod(): Option[String] = None
  def betterBackupMethod(): Option[String] = Some("A valid result")
  val betterChainedResult = betterUnsafeMethod() orElse betterBackupMethod()
  println(betterChainedResult)

  // functions on Options
  println(myFirstOption.isEmpty)
  println(myFirstOption.get) // Unsafe - DO NOT USE THIS
  // println(noOption.get) // NoSuchElementException

  // map, flatMap, filter
  println("map: " + myFirstOption.map(_ * 2))
  println("map: "+ noOption.map(_ * 2))
  println("flatMap: " + myFirstOption.flatMap(x => Option(x * 2)))
  println("flatMap: " + noOption.flatMap(x => Option(x * 2)))

  println(myFirstOption.filter(x => x > 10))
  println(myFirstOption.flatMap(x => Option(x * 10)))

  // for-comprehensions
  /*
    Exercise.
   */
  val config: Map[String, String] = Map(
    // fetched from elsewhere
    "host" -> "176.45.36.1",
    "port" -> "80"
  )

  class Connection {
    def connect = "Connected" // connect to some server
  }

  object Connection {
    val random = new Random(System.nanoTime())

    def apply(host: String, port: String): Option[Connection] =
      if (random.nextBoolean()) Some(new Connection)
      else None
  }

  // My solution
  def isConnected ={
    val host = config.get("host")
    val port = config.get("port")
    if (!host.isEmpty && !port.isEmpty) {
      val connection = Connection.apply(host.get, port.get)
      if (!connection.isEmpty)
        println(connection.get.connect)
      else
        println("Not connection")

    }
    else
      println("Not port or host")
  }

  //isConnected

  // Video Solution
  val host = config.get("host")
  val port = config.get("port")

  /*
    if (h != null)
      if (p != null)
        return Connection.apply(h, p)
    return null
   */
  val connection = host.flatMap(h => port.flatMap(p => Connection.apply(h, p)))
  /*
    if (c != null)
      return c.connect
    return null
   */
  val connectionStatus = connection.map(c => c.connect)
  // if (connectionStatus == null) println(None) else print (Some(connectionstatus.get))
  //println(connectionStatus)
  /*
    if (status != null)
      println(status)
   */
  //connectionStatus.foreach(println)

  //println(config.get("host").flatMap(host => config.get("port").flatMap(port => Connection.apply(host, port).map(conection => conection.connect))))

  //config.get("host").flatMap(host => config.get("port").flatMap(port => Connection.apply(host, port).map(conection => println(conection.connect))))

  // for comprehensions
  val forConnectionStatus = for {
    host <- config.get("host")
    port <- config.get("port")
    connection <- Connection(host, port)
  } yield connection.connect

  println(forConnectionStatus)
  forConnectionStatus.foreach(println)

}
