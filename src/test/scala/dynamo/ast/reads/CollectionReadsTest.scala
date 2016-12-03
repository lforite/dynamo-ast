package dynamo.ast.reads

import dynamo.ast.Arbitraries._
import dynamo.ast._
import org.specs2.{ScalaCheck, Specification}

class CollectionReadsTest  extends Specification with ScalaCheck { def is = s2"""
 Specification for the collection reads

  Reading any L as List should yield a success containing the List $readList
  Reading any DynamoType which is not a L as List should yield an error $readListFail

  Reading any SS as Set should yield a success containing the Set $readSetSS
  Reading any NS as Set should yield a success containing the Set $readSetNS
  Reading any L as Set should yield a success containing the List $readSetL
  Reading any DynamoType which is not a L as List should yield an error $readSetFail
  """

  def readList = prop { l: L[DynamoType] =>
    DynamoRead.ListRead[DynamoType].read(l) should_== DynamoReadSuccess(l.elements)
  }

  def readListFail = prop { dynamoType: DynamoType =>
    DynamoRead.ListRead[DynamoType].read(dynamoType) should beLike { case DynamoReadError(_, err) => err must contain("was expecting L got") }
  }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
    case l: L[_] => false
    case _ => true
  })


  def readSetSS = prop { ss: SS =>
    DynamoRead.SetRead[DynamoType].read(ss) should_== DynamoReadSuccess(ss.strings)
  }

  def readSetNS = prop { ns: NS =>
    DynamoRead.SetRead[DynamoType].read(ns) should_== DynamoReadSuccess(ns.numbers)
  }

  def readSetL = prop { l: L[DynamoType] =>
    DynamoRead.SetRead[DynamoType].read(l) should beLike { case DynamoReadSuccess(_) => ok }
  }

  def readSetFail = prop { dynamoType: DynamoType =>
    DynamoRead.SetRead[DynamoType].read(dynamoType) should beLike { case DynamoReadError(_, err) => ok }
  }.setGen(Arbitraries.DynamoTypeArb.arbitrary.filter {
    case l: L[_] => false
    case ns: NS => false
    case ss: SS => false
    case _ => true
  })

}
