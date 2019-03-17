package dynamo.ast.reads

import dynamo.ast.Arbitraries._
import dynamo.ast._
import org.specs2.{ScalaCheck, Specification}

class DefaultReadsTest extends Specification with ScalaCheck {
  def is = s2"""
 Specification for the default reads
  Reading any DynamoType as a DynamoType should yield the type itself $readDynamoType
  Reading any S as S should yield a success containing the S $readS
  Reading any DynamoType which is not a S as S should yield an error $readSFail
  Reading any N as N should yield a success containing the S $readN
  Reading any DynamoType which is not a N as N should yield an error $readNFail
  Reading any BOOL as BOOL should yield a success containing the BOOL $readBOOL
  Reading any DynamoType which is not a BOOL as BOOL should yield an error $readBOOLFail
  Reading any L as L should yield a success containing the L $readL
  Reading any DynamoType which is not a L as L should yield an error $readLFail
  Reading any M as M should yield a success containing the M $readM
  Reading any DynamoType which is not a M as M should yield an error $readMFail
  Reading any NS as NS should yield a success containing the NS $readNS
  Reading any DynamoType which is not a NS as NS should yield an error $readNSFail
  Reading any SS as SS should yield a success containing the SS $readSS
  Reading any DynamoType which is not a SS as SS should yield an error $readSSFail
  Reading any NULL as NULL should yield a success containing the NULL $readNULL
  Reading any DynamoType which is not a NULL as NULL should yield an error $readNULLFail
  """

  def readDynamoType = prop { dynamoType: DynamoType =>
    DynamoRead.DynamoTypeRead.read(dynamoType) should_== DynamoReadSuccess(dynamoType)
  }

  def readS = prop { s: S =>
    DynamoRead.SRead.read(s) should_== DynamoReadSuccess(s)
  }

  def readSFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.SRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting S got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: S => false
      case _    => true
    })

  def readN = prop { n: N =>
    DynamoRead.NRead.read(n) should_== DynamoReadSuccess(n)
  }

  def readNFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.NRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting N got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: N => false
      case _    => true
    })

  def readBOOL = prop { bool: BOOL =>
    DynamoRead.BOOLRead.read(bool) should_== DynamoReadSuccess(bool)
  }

  def readBOOLFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.BOOLRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting BOOL got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: BOOL => false
      case _       => true
    })

  def readL = prop { l: L[DynamoType] =>
    DynamoRead.LRead[DynamoType].read(l) should_== DynamoReadSuccess(l)
  }

  def readLFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.LRead[DynamoType].read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting L got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: L[_] => false
      case _       => true
    })

  def readM = prop { m: M =>
    DynamoRead.MRead.read(m) should_== DynamoReadSuccess(m)
  }

  def readMFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.MRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting M got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: M => false
      case _    => true
    })

  def readNS = prop { ns: NS =>
    DynamoRead.NSRead.read(ns) should_== DynamoReadSuccess(ns)
  }

  def readNSFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.NSRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting NS got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: NS => false
      case _     => true
    })

  def readSS = prop { ss: SS =>
    DynamoRead.SSRead.read(ss) should_== DynamoReadSuccess(ss)
  }

  def readSSFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.SSRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting SS got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: SS => false
      case _     => true
    })

  def readNULL =
    DynamoRead.NULLRead.read(NULL) should_== DynamoReadSuccess(NULL)

  def readNULLFail =
    prop { dynamoType: DynamoType =>
      DynamoRead.NULLRead.read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting NULL got") }
    }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
      case _: NULL.type => false
      case _            => true
    })
}
