package dynamo.ast

import dynamo.ast.reads.{DynamoRead, DynamoReadError, DynamoReadResult, DynamoReadSuccess}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._

object Arbitraries {
  implicit val SArb: Arbitrary[S] = Arbitrary[S] { SGen }
  implicit val NArb: Arbitrary[N] = Arbitrary[N] { NGen }
  implicit val BOOLArb: Arbitrary[BOOL] = Arbitrary[BOOL] { BOOLGen }
  implicit val MArb: Arbitrary[M] = Arbitrary[M] { MGen }
  implicit val LArb: Arbitrary[L[DynamoType]] = Arbitrary[L[DynamoType]] { LGen }
  implicit val NSArb: Arbitrary[NS] = Arbitrary[NS] { NSGen }
  implicit val SSArb: Arbitrary[SS] = Arbitrary[SS] { SSGen }
  implicit val NULLArb: Arbitrary[NULL.type] = Arbitrary[NULL.type] { NULLGen }
  implicit val DynamoTypeArb: Arbitrary[DynamoType] = Arbitrary[DynamoType] { DynamoTypeGen }

  def DynamoTypeGen: Gen[DynamoType] = frequency(
    (5,
      delay(SimpleTypeGen)),
    (1, delay(oneOf(LGen, NSGen, SSGen, MGen, NULLGen)))
  )

  def SGen = arbitrary[String] map S
  def NGen = arbitrary[Number] map (i => N(i.toString))
  def BOOLGen = arbitrary[Boolean] map BOOL
  def MGen = listOfN(size, kvGen) map M
  def LGen: Gen[L[DynamoType]] = listOfN(size, DynamoTypeGen) map L.apply
  def NSGen = listOfN(size, NGen) map (s => NS(s.toSet))
  def SSGen = listOfN(size, SGen) map (s => SS(s.toSet))
  def NULLGen = const(NULL)

  def SimpleTypeGen: Gen[DynamoScalarType] = oneOf(SGen, NGen, BOOLGen)

  def kvGen: Gen[(String, DynamoType)] = for {
    key <- identifier
    value <- DynamoTypeGen
  } yield (key, value)


  private def size = choose(0, 5).sample.get


  implicit val DynamoReadArb: Arbitrary[DynamoReadResult[DynamoType]] = Arbitrary[DynamoReadResult[DynamoType]] { DynamoReadResultGen }
  implicit val DynamoReadSuccessArb: Arbitrary[DynamoReadSuccess[DynamoType]] = Arbitrary[DynamoReadSuccess[DynamoType]] { DynamoReadSuccessGen }
  implicit val DynamoListReadSuccessfulArb: Arbitrary[List[DynamoRead[DynamoType]]] = Arbitrary[List[DynamoRead[DynamoType]]] { listOfN(size, DynamoReadSuccessfulGen) }

  def DynamoReadSuccessfulGen = DynamoReadSuccessGen map DynamoRead.lift
  def DynamoReadResultGen = oneOf(DynamoReadSuccessGen, DynamoReadErrorGen)
  def DynamoReadSuccessGen = DynamoTypeGen map DynamoReadSuccess.apply
  def DynamoReadErrorGen = for {
    path <- identifier
    error <- arbitrary[String]
  } yield DynamoReadError(path, error)
}