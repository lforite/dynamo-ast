package dynamo.ast.implicits

object StringOps {
  implicit class StringNumberOps(value: String) {
    private def validNumber[A](f: String => A): Boolean = try { f(value); true } catch { case _: NumberFormatException => false }
    def isValidInt: Boolean    = validNumber(_.toInt)
    def isValidShort: Boolean  = validNumber(_.toShort)
    def isValidByte: Boolean   = validNumber(_.toByte)
    def isValidLong: Boolean   = validNumber(_.toLong)
    def isValidFloat: Boolean  = validNumber(_.toFloat)
    def isValidDouble: Boolean = validNumber(_.toDouble)
  }
}
