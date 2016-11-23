package dynamo.ast.implicits

object StringOps {
  implicit class StringNumberOps(value: String) {
    private def validNumber[A](f : (String => A)): Boolean = try { f(value); true } catch { case t: NumberFormatException => false }
    def isValidInt = validNumber(_.toInt)
    def isValidShort = validNumber(_.toShort)
    def isValidByte = validNumber(_.toByte)
    def isValidLong = validNumber(_.toLong)
    def isValidFloat = validNumber(_.toFloat)
    def isValidDouble = validNumber(_.toDouble)
  }
}

