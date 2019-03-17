package dynamo.ast.reads

import dynamo.ast._
import org.specs2.{ScalaCheck, Specification}
import dynamo.ast.Arbitraries._

class PrimitiveReadTest extends Specification with ScalaCheck {
  def is = s2"""
 Specification for the default reads
  Reading any S as a String should yield a success containing the string $readString
  Reading any DynamoType which is not a S as String should yield an error $readStringFail

  Reading any N containing a valid Int as Int should yield a success containing the Int $readInt
  Reading any N containing an invalid Int as Int should yield an error $readIntInvalid
  Reading any DynamoType which is not a N as Int should yield an error $readIntFail

  Reading any N containing a valid Short as Short should yield a success containing the Short $readShort
  Reading any N containing an invalid Short as Short should yield an error $readShortInvalid
  Reading any DynamoType which is not a N as Short should yield an error $readShortFail

  Reading any N containing a valid Byte as Byte should yield a success containing the Byte $ok
  Reading any N containing an invalid Byte as Byte should yield an error $ok
  Reading any DynamoType which is not a N as Byte should yield an error $ok

  Reading any N containing a valid Long as Long should yield a success containing the Long $readLong
  Reading any N containing an invalid Long as Long should yield an error $readLongInvalid
  Reading any DynamoType which is not a N as Long should yield an error $readLongFail

  Reading any N containing a valid Float as Float should yield a success containing the Float $readFloat
  Reading any N containing an invalid Float as Float should yield an error $readFloatInvalid
  Reading any DynamoType which is not a N as Float should yield an error $readFloatFail

  Reading any N containing a valid Double as Double should yield a success containing the Double $readDouble
  Reading any N containing an invalid Double as Double should yield an error $readDoubleInvalid
  Reading any DynamoType which is not a N as Double should yield an error $readDoubleFail

  Reading any BOOL as a Boolean should yield a success containing the Boolean $readBoolean
  Reading any DynamoType which is not a BOOL as Boolean should yield an error $readBooleanFail
 """

  def readString = prop { s: S =>
    DynamoRead.StringRead.read(s) should_== DynamoReadSuccess(s.value)
  }

  def readStringFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.StringRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting S got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: S => false
      case _    => true
    })

  def readInt = prop { i: Int =>
    DynamoRead.IntRead.read(N(i.toString)) should_== DynamoReadSuccess(i)
  }

  def readIntInvalid = prop { i: Int =>
    DynamoRead.IntRead.read(N(i.toString + "INVALID")) should beLike { case DynamoReadError(_, err) => err must contain("expected valid int got") }
  }

  def readIntFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.IntRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting N got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: N => false
      case _    => true
    })

  def readShort = prop { s: Short =>
    DynamoRead.ShortRead.read(N(s.toString)) should_== DynamoReadSuccess(s)
  }

  def readShortInvalid = prop { s: Short =>
    DynamoRead.ShortRead.read(N(s.toString + "INVALID")) should beLike { case DynamoReadError(_, err) => err must contain("expected valid short got") }
  }

  def readShortFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.ShortRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting N got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: N => false
      case _    => true
    })

  //TODO later: Add suport for byte type
//
//  def readShort = prop { s: Short =>
//    DynamoRead.ShortRead.read(N(s.toString)) should_== DynamoReadSuccess(s)
//  }
//
//  def readShortInvalid = prop { s: Short =>
//    DynamoRead.ShortRead.read(N(s.toString + "INVALID")) should beLike { case DynamoReadError(_, err) => err must contain("expected valid short got") }
//  }
//
//  def readShortFail = prop { dynamoType: DynamoType =>
//    DynamoRead.ShortRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting N got") }
//  }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
//    case n: N => false
//    case _ => true
//  })

  def readLong = prop { l: Long =>
    DynamoRead.LongRead.read(N(l.toString)) should_== DynamoReadSuccess(l)
  }

  def readLongInvalid = prop { l: Long =>
    DynamoRead.LongRead.read(N(l.toString + "INVALID")) should beLike { case DynamoReadError(_, err) => err must contain("expected valid long got") }
  }

  def readLongFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.LongRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting N got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: N => false
      case _    => true
    })

  def readFloat = prop { f: Float =>
    DynamoRead.FloatRead.read(N(f.toString)) should_== DynamoReadSuccess(f)
  }

  def readFloatInvalid = prop { f: Float =>
    DynamoRead.FloatRead.read(N(f.toString + "INVALID")) should beLike { case DynamoReadError(_, err) => err must contain("expected valid float got") }
  }

  def readFloatFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.FloatRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting N got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: N => false
      case _    => true
    })

  def readDouble = prop { d: Double =>
    DynamoRead.DoubleRead.read(N(d.toString)) should_== DynamoReadSuccess(d)
  }

  def readDoubleInvalid = prop { d: Double =>
    DynamoRead.DoubleRead.read(N(d.toString + "INVALID")) should beLike { case DynamoReadError(_, err) => err must contain("expected valid double got") }
  }

  def readDoubleFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.DoubleRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting N got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: N => false
      case _    => true
    })

  def readBoolean = prop { b: Boolean =>
    DynamoRead.BooleanRead.read(BOOL(b)) should_== DynamoReadSuccess(b)
  }

  def readBooleanFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.BooleanRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting BOOL got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: BOOL => false
      case _       => true
    })

}
