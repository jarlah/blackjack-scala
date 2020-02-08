import scala.util.Try

package object game {
  def readInt(msg: String): Int = {
    print(msg)
    Try(scala.io.StdIn.readInt()).getOrElse(0)
  }

  def readLine(msg: String): String = {
    print(msg)
    scala.io.StdIn.readLine()
  }

  def getAnswer(question: String, possibleAnswers: List[String]): String = {
    var input = ""
    while (!possibleAnswers.contains(input)) {
      input = readLine(s"$question? Enter one of (${possibleAnswers.mkString(",")}): ").toLowerCase
    }
    input
  }
}
