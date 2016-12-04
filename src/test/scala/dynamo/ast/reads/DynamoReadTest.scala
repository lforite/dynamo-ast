package dynamo.ast.reads

import dynamo.ast.Arbitraries._
import dynamo.ast.{DynamoType, M, S}
import org.specs2._

class DynamoReadTest extends Specification with ScalaCheck { def is = s2"""
 Specification for DynamoRead
  pure returns a success $pure

  lift should lift the result into a read $lift

  sequencing a list of successes should yield a success $sequenceOk
  sequencing a list of successes with a single error should yield an error $sequenceKo

  reading to an existing path should yield the value $read
  reading to a non existing path should yield an error $readPathNotFound
  read[].at() should be equivalent to read().as[] $readAsReadAt

  reading field as an optional field that exists should yield Some $readOpt
  reading field as an optional field that does not exist should yield None $readOptNone
  readOpt[].at() should be equivalent to readOpt().as[] $readOptAsReadOptAt
  """

  def pure = prop { (dynamoType: DynamoType, any: DynamoType) =>
    DynamoRead.pure(dynamoType).read(any) should_== DynamoReadSuccess(dynamoType)
  }

  def lift = prop { (dynamoReadResult: DynamoReadResult[DynamoType], any: DynamoType) =>
    DynamoRead.lift(dynamoReadResult).read(any) should_== dynamoReadResult
  }

  def sequenceOk = prop { (results: List[DynamoRead[DynamoType]], any: DynamoType) =>
    DynamoRead.sequence(results).read(any) match {
      case DynamoReadSuccess(_) => ok("Successfully sequenced")
      case DynamoReadError(p, e) => ko("Was expecting sequence to yield a success")
    }
  }

  def sequenceKo = prop { (results: List[DynamoRead[DynamoType]], any: DynamoType) =>
    val errorRead = DynamoRead.lift[DynamoType](DynamoReadError("any_path", "Nasty error !"))
    DynamoRead.sequence(errorRead :: results).read(any) match {
      case DynamoReadSuccess(_) => ko("Was expecting sequence to yield an error")
      case DynamoReadError(p, e) =>
        p should_== "any_path"
        e should_== "Nasty error !"
    }
  }

  def read = {
    val dynamoType = M(List("key" -> S("any string")))
    DynamoRead.read("key").as[S].read(dynamoType) should_== DynamoReadSuccess(S("any string"))
  }

  def readPathNotFound = {
    val dynamoType = M(List("key" -> S("any string")))
    DynamoRead.read("path_not_found").as[S].read(dynamoType) should_== DynamoReadError("path_not_found", "Path not found")
  }

  def readNotM = {
    val dynamoType = S("any string")
    DynamoRead.read("key").as[S].read(dynamoType) should beLike { case DynamoReadError("", err) => err must contain("was expecting M got")}
  }

  def readAsReadAt = prop { (any: DynamoType, other: DynamoType, path: String) =>
    import DynamoRead._
    DynamoRead.read(path).as[DynamoType].read(other) should_== DynamoRead.read[DynamoType].at(path).read(other)
  }

  def readOpt = {
    val dynamoType = M(List("key" -> S("any string")))
    DynamoRead.readOpt("key").as[S].read(dynamoType) should_== DynamoReadSuccess(Some(S("any string")))
  }

  def readOptNone = {
    val dynamoType = M(List())
    DynamoRead.readOpt("key").as[S].read(dynamoType) should_== DynamoReadSuccess(None)
  }

  def readOptAsReadOptAt = prop { (any: DynamoType, other: DynamoType, path: String) =>
    import DynamoRead._
    DynamoRead.readOpt(path).as[DynamoType].read(other) should_== DynamoRead.readOpt[DynamoType].at(path).read(other)
  }
}


