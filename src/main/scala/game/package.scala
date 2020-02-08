package object game {
  def readInt(msg: String): Int = {
    print(msg)
    scala.io.StdIn.readInt()
  }

  def readLine(msg: String): String = {
    print(msg)
    scala.io.StdIn.readLine()
  }

  def continue(msg: String): Boolean =
    readLine(msg).toLowerCase.toList.headOption.map(_.toLower).contains('y')
}
