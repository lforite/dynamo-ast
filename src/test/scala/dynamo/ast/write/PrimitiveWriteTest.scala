package dynamo.ast.write

import dynamo.ast._
import org.specs2.{ScalaCheck, Specification}

class PrimitiveWriteTest extends Specification with ScalaCheck { def is = s2"""
 Specification for the primitive writes
  Writing any String should yield a success containing an S containing the String $writeString
  Writing any Int should yield a success containing an N containing the Int as String $writeInt
  Writing any Short should yield a success containing an N containing the Short as String $writeShort
  Writing any Long should yield a success containing an N containing the Long as String $writeLong
  Writing any Float should yield a success containing an N containing the Float as String $writeFloat
  Writing any Double should yield a success containing an N containing the Double as String $writeDouble
  Writing any Boolean should yield a success containing an BOOL containing the Boolean as String $writeBoolean
"""

  def writeString = prop { string: String ⇒
    DynamoWrite.StringWrite.write(string) should_== S(string)
  }

  def writeInt = prop { int: Int ⇒
    DynamoWrite.IntWrite.write(int) should_== N(int.toString)
  }

  def writeShort = prop { short: Short ⇒
    DynamoWrite.ShortWrite.write(short) should_== N(short.toString)
  }

  def writeLong = prop { long: Long ⇒
    DynamoWrite.LongWrite.write(long) should_== N(long.toString)
  }

  def writeFloat = prop { float: Float ⇒
    DynamoWrite.FloatWrite.write(float) should_== N(float.toString)
  }

  def writeDouble = prop { double: Double ⇒
    DynamoWrite.DoubleWrite.write(double) should_== N(double.toString)
  }

  def writeBoolean = prop { boolean: Boolean ⇒
    DynamoWrite.BooleanWrite.write(boolean) should_== BOOL(boolean)
  }
}
