package dynamo.ast.writes

import dynamo.ast.{M, S}
import org.specs2.Specification

class DynamoWriteTest extends Specification { def is = s2"""
  Tests for DynamoWrite
    writing to a valid path should yield an M with the corresponding type $write
    writing to a valid path an Option should yield an M with the corresponding type $writeOptPresent
    writing to a valid path an empty Option should yield an empty M $writeOptAbsent
"""

  def write = {
    DynamoWrite.write[String].at("test").write("test_string") should_== M(List("test" -> S("test_string")))
  }

  def writeOptPresent = {
    DynamoWrite.writeOpt[String].at("test").write(Some("test_string")) should_== M(List("test" -> S("test_string")))
  }

  def writeOptAbsent = {
    DynamoWrite.writeOpt[String].at("test").write(None) should_== M(List())
  }
}
