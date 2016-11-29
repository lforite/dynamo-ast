package dynamo.ast.reads

import dynamo.ast._

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import DynamoRead._

trait DynamoRead[A] { self =>

  def read(awsType: DynamoType): DynamoReadResult[A]

  def map[B](f: (A => B)): DynamoRead[B] = DynamoRead[B] { awsType => self.read(awsType).map(f) }

  def flatMap[B](f: (A => DynamoRead[B])): DynamoRead[B] = DynamoRead[B] { awsType =>
    self.read(awsType) match {
      case DynamoReadSuccess(a) => f(a).read(awsType)
      case e: DynamoReadError => e
    }
  }
}

object DynamoRead extends DefaultReads with PrimitiveRead with CollectionRead {

  def pure[A](a: A): DynamoRead[A] = DynamoRead[A] { _ => DynamoReadSuccess(a) }

  def lift[A](a: DynamoReadResult[A]) = DynamoRead[A] { _ => a }

  def sequence[A, MO[X] <: TraversableOnce[X]](in: MO[DynamoRead[A]])(implicit cbf: CanBuildFrom[MO[DynamoRead[A]], A, MO[A]]): DynamoRead[MO[A]] = {
    in.foldLeft(pure(cbf(in)))((acc, next) => for (acc2 <- acc; next2 <- next) yield acc2 += next2).map(_.result())
  }

  def apply[A](f: (DynamoType => DynamoReadResult[A])): DynamoRead[A] = new DynamoRead[A] {
    def read(awsType: DynamoType): DynamoReadResult[A] = f(awsType)
  }

  def read[A]: ReadAt[A] = new ReadAt[A]

  class ReadAt[A] {
    def at(path: String)(implicit reads: DynamoRead[A]): DynamoRead[A] = DynamoRead[A] {
      MRead.read(_).flatMap { m: M =>
        m.elements.find(_._1 == path).map(_._2).fold[DynamoReadResult[A]](DynamoReadError(path, "Path not found"))(me => reads.read(me).withPath(path))
      }
    }
  }

  def read(path: String) = new ReadAs(path)

  class ReadAs(at: String) {
    def as[A](implicit reads: DynamoRead[A]): DynamoRead[A] = DynamoRead[A] {
      MRead.read(_).flatMap { m: M =>
        m.elements.find(_._1 == at).map(_._2).fold[DynamoReadResult[A]](DynamoReadError(at, "Path not found"))(me => reads.read(me).withPath(at))
      }
    }
  }

}

trait DefaultReads {

  implicit object DynamoTypeRead extends DynamoRead[DynamoType] {
    override def read(awsType: DynamoType): DynamoReadResult[DynamoType] = DynamoReadSuccess(awsType)
  }

  implicit object SRead extends DynamoRead[S] {
    override def read(awsType: DynamoType): DynamoReadResult[S] = awsType match {
      case s: S => DynamoReadSuccess(s)
      case e => DynamoReadError("", s"was expecting S got $e")
    }
  }

  implicit object NRead extends DynamoRead[N] {
    override def read(awsType: DynamoType): DynamoReadResult[N] = awsType match {
      case n: N => DynamoReadSuccess(n)
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object BOOLReads extends DynamoRead[BOOL] {
    override def read(awsType: DynamoType): DynamoReadResult[BOOL] = awsType match {
      case b: BOOL => DynamoReadSuccess(b)
      case e => DynamoReadError("", s"was expecting BOOL got $e")
    }
  }

  implicit def lRead[A <: DynamoType](implicit ra: DynamoRead[A]): DynamoRead[L[A]] = DynamoRead[L[A]] {
    case awsType@(l@L(e)) => sequence(e.map(a => lift(ra.read(a)))).read(awsType).map(L.apply)
    case e => DynamoReadError("", s"was expecting L got $e")
  }

  implicit object MRead extends DynamoRead[M] {
    override def read(awsType: DynamoType): DynamoReadResult[M] = awsType match {
      case m: M => DynamoReadSuccess(m)
      case e => DynamoReadError("", s"was expecting M got $e")
    }
  }

  implicit object NSRead extends DynamoRead[NS] {
    override def read(awsType: DynamoType): DynamoReadResult[NS] = awsType match {
      case ns: NS => DynamoReadSuccess(ns)
      case e => DynamoReadError("", s"was expecting NS got $e")
    }
  }

  implicit object SSRead extends DynamoRead[SS] {
    override def read(awsType: DynamoType): DynamoReadResult[SS] = awsType match {
      case ss: SS => DynamoReadSuccess(ss)
      case e => DynamoReadError("", s"was expecting NS got $e")
    }
  }

  implicit object NULLRead extends DynamoRead[NULL.type] {
    override def read(awsType: DynamoType): DynamoReadResult[NULL.type] = awsType match {
      case nul: NULL.type => DynamoReadSuccess(NULL)
      case e => DynamoReadError("", s"was expecting NULL got $e")
    }
  }

}

trait PrimitiveRead {

  import dynamo.ast.implicits.StringOps._

  implicit object StringRead extends DynamoRead[String] {
    override def read(awsType: DynamoType): DynamoReadResult[String] = awsType match {
      case S(value) => DynamoReadSuccess(value)
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object IntRead extends DynamoRead[Int] {
    override def read(awsType: DynamoType): DynamoReadResult[Int] = awsType match {
      case N(value) if value.isValidInt => DynamoReadSuccess(value.toInt)
      case N(value) => DynamoReadError("", s"expected valid int got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object ShortRead extends DynamoRead[Short] {
    override def read(awsType: DynamoType): DynamoReadResult[Short] = awsType match {
      case N(value) if value.isValidShort => DynamoReadSuccess(value.toShort)
      case N(value) => DynamoReadError("", s"expected valid short got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object ByteRead extends DynamoRead[Byte] {
    override def read(awsType: DynamoType): DynamoReadResult[Byte] = awsType match {
      case N(value) if value.isValidByte => DynamoReadSuccess(value.toByte)
      case N(value) => DynamoReadError("", s"expected valid byte got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object LongRead extends DynamoRead[Long] {
    override def read(awsType: DynamoType): DynamoReadResult[Long] = awsType match {
      case N(value) if value.isValidLong => DynamoReadSuccess(value.toLong)
      case N(value) => DynamoReadError("", s"expected valid long got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object FloatRead extends DynamoRead[Float] {
    override def read(awsType: DynamoType): DynamoReadResult[Float] = awsType match {
      case N(value) if value.isValidFloat => DynamoReadSuccess(value.toFloat)
      case N(value) => DynamoReadError("", s"expected valid float got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object DoubleRead extends DynamoRead[Double] {
    override def read(awsType: DynamoType): DynamoReadResult[Double] = awsType match {
      case N(value) if value.isValidDouble => DynamoReadSuccess(value.toDouble)
      case N(value) => DynamoReadError("", s"expected valid double got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

}

trait CollectionRead { self: PrimitiveRead =>
  implicit def listRead[A](implicit ra: DynamoRead[A]): DynamoRead[List[A]] = DynamoRead[List[A]] {
    case awsType@(l@L(e)) => sequence(e.map(a => lift(ra.read(a)))).read(awsType)
    case e => DynamoReadError("", s"was expecting L got $e")
  }

  implicit def setRead[A](implicit ra: DynamoRead[A]): DynamoRead[Set[A]] = DynamoRead[Set[A]] {
    case awsType@(SS(e)) => sequence(e.map(a => lift(ra.read(a)))).read(awsType)
    case awsType@(NS(e)) => sequence(e.map(a => lift(ra.read(a)))).read(awsType)
    case awsType@(L(e)) => sequence(e.map(a => lift(ra.read(a))).toSet).read(awsType)
    case e => DynamoReadError("", s"was expecting SS, NS or L got $e")
  }

  implicit def mapRead[A](implicit ra: DynamoRead[A]): DynamoRead[Map[String, A]] = DynamoRead[Map[String, A]] {
    case awsType@(M(e)) => sequence(e.map(a => lift(ra.read(a._2).map(r => (a._1, r))))).read(awsType).map(_.toMap)
    case e => DynamoReadError("", s"was expecting M got $e")
  }

}

