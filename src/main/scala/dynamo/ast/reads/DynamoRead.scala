package dynamo.ast.reads

import dynamo.ast._

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import DynamoRead._

trait DynamoRead[A] { self =>

  def read(dynamoType: DynamoType): DynamoReadResult[A]

  def map[B](f: (A => B)): DynamoRead[B] = DynamoRead[B] { dynamoType => self.read(dynamoType).map(f) }

  def flatMap[B](f: (A => DynamoRead[B])): DynamoRead[B] = DynamoRead[B] { dynamoType =>
    self.read(dynamoType) match {
      case DynamoReadSuccess(a) => f(a).read(dynamoType)
      case e: DynamoReadError => e
    }
  }
}

object DynamoRead extends DefaultReads with PrimitiveReads with CollectionRead {

  def pure[A](a: A): DynamoRead[A] = DynamoRead[A] { _ => DynamoReadSuccess(a) }

  def lift[A](a: DynamoReadResult[A]) = DynamoRead[A] { _ => a }

  def sequence[A, MO[X] <: TraversableOnce[X]](in: MO[DynamoRead[A]])(implicit cbf: CanBuildFrom[MO[DynamoRead[A]], A, MO[A]]): DynamoRead[MO[A]] = {
    in.foldLeft(pure(cbf(in)))((acc, next) => for (acc2 <- acc; next2 <- next) yield acc2 += next2).map(_.result())
  }

  def apply[A](f: (DynamoType => DynamoReadResult[A])): DynamoRead[A] = new DynamoRead[A] {
    def read(dynamoType: DynamoType): DynamoReadResult[A] = f(dynamoType)
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
    override def read(dynamoType: DynamoType): DynamoReadResult[DynamoType] = DynamoReadSuccess(dynamoType)
  }

  implicit object SRead extends DynamoRead[S] {
    override def read(dynamoType: DynamoType): DynamoReadResult[S] = dynamoType match {
      case s: S => DynamoReadSuccess(s)
      case e => DynamoReadError("", s"was expecting S got $e")
    }
  }

  implicit object NRead extends DynamoRead[N] {
    override def read(dynamoType: DynamoType): DynamoReadResult[N] = dynamoType match {
      case n: N => DynamoReadSuccess(n)
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object BOOLRead extends DynamoRead[BOOL] {
    override def read(dynamoType: DynamoType): DynamoReadResult[BOOL] = dynamoType match {
      case b: BOOL => DynamoReadSuccess(b)
      case e => DynamoReadError("", s"was expecting BOOL got $e")
    }
  }

  implicit def LRead[A <: DynamoType](implicit ra: DynamoRead[A]): DynamoRead[L[A]] = DynamoRead[L[A]] {
    case dynamoType@(l@L(e)) => sequence(e.map(a => lift(ra.read(a)))).read(dynamoType).map(L.apply)
    case e => DynamoReadError("", s"was expecting L got $e")
  }

  implicit object MRead extends DynamoRead[M] {
    override def read(dynamoType: DynamoType): DynamoReadResult[M] = dynamoType match {
      case m: M => DynamoReadSuccess(m)
      case e => DynamoReadError("", s"was expecting M got $e")
    }
  }

  implicit object NSRead extends DynamoRead[NS] {
    override def read(dynamoType: DynamoType): DynamoReadResult[NS] = dynamoType match {
      case ns: NS => DynamoReadSuccess(ns)
      case e => DynamoReadError("", s"was expecting NS got $e")
    }
  }

  implicit object SSRead extends DynamoRead[SS] {
    override def read(dynamoType: DynamoType): DynamoReadResult[SS] = dynamoType match {
      case ss: SS => DynamoReadSuccess(ss)
      case e => DynamoReadError("", s"was expecting SS got $e")
    }
  }

  implicit object NULLRead extends DynamoRead[NULL.type] {
    override def read(dynamoType: DynamoType): DynamoReadResult[NULL.type] = dynamoType match {
      case nul: NULL.type => DynamoReadSuccess(NULL)
      case e => DynamoReadError("", s"was expecting NULL got $e")
    }
  }

}

trait PrimitiveReads {

  import dynamo.ast.implicits.StringOps._

  implicit object StringRead extends DynamoRead[String] {
    override def read(dynamoType: DynamoType): DynamoReadResult[String] = dynamoType match {
      case S(value) => DynamoReadSuccess(value)
      case e => DynamoReadError("", s"was expecting S got $e")
    }
  }

  implicit object IntRead extends DynamoRead[Int] {
    override def read(dynamoType: DynamoType): DynamoReadResult[Int] = dynamoType match {
      case N(value) if value.isValidInt => DynamoReadSuccess(value.toInt)
      case N(value) => DynamoReadError("", s"expected valid int got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object ShortRead extends DynamoRead[Short] {
    override def read(dynamoType: DynamoType): DynamoReadResult[Short] = dynamoType match {
      case N(value) if value.isValidShort => DynamoReadSuccess(value.toShort)
      case N(value) => DynamoReadError("", s"expected valid short got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object ByteRead extends DynamoRead[Byte] {
    override def read(dynamoType: DynamoType): DynamoReadResult[Byte] = dynamoType match {
      case N(value) if value.isValidByte => DynamoReadSuccess(value.toByte)
      case N(value) => DynamoReadError("", s"expected valid byte got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object LongRead extends DynamoRead[Long] {
    override def read(dynamoType: DynamoType): DynamoReadResult[Long] = dynamoType match {
      case N(value) if value.isValidLong => DynamoReadSuccess(value.toLong)
      case N(value) => DynamoReadError("", s"expected valid long got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object FloatRead extends DynamoRead[Float] {
    override def read(dynamoType: DynamoType): DynamoReadResult[Float] = dynamoType match {
      case N(value) if value.isValidFloat => DynamoReadSuccess(value.toFloat)
      case N(value) => DynamoReadError("", s"expected valid float got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object DoubleRead extends DynamoRead[Double] {
    override def read(dynamoType: DynamoType): DynamoReadResult[Double] = dynamoType match {
      case N(value) if value.isValidDouble => DynamoReadSuccess(value.toDouble)
      case N(value) => DynamoReadError("", s"expected valid double got $value")
      case e => DynamoReadError("", s"was expecting N got $e")
    }
  }

  implicit object BooleanRead extends DynamoRead[Boolean] {
    override def read(dynamoType: DynamoType): DynamoReadResult[Boolean] = dynamoType match {
      case BOOL(value) => DynamoReadSuccess(value)
      case e => DynamoReadError("", s"was expecting BOOL got $e")
    }
  }
}

trait CollectionRead { self: PrimitiveReads =>
  implicit def ListRead[A](implicit ra: DynamoRead[A]): DynamoRead[List[A]] = DynamoRead[List[A]] {
    case dynamoType@(l@L(e)) => sequence(e.map(a => lift(ra.read(a)))).read(dynamoType)
    case e => DynamoReadError("", s"was expecting L got $e")
  }

  implicit def SetRead[A](implicit ra: DynamoRead[A]): DynamoRead[Set[A]] = DynamoRead[Set[A]] {
    case dynamoType@(SS(e)) => sequence(e.map(a => lift(ra.read(a)))).read(dynamoType)
    case dynamoType@(NS(e)) => sequence(e.map(a => lift(ra.read(a)))).read(dynamoType)
    case dynamoType@(L(e)) => sequence(e.map(a => lift(ra.read(a))).toSet).read(dynamoType)
    case e => DynamoReadError("", s"was expecting SS, NS or L got $e")
  }

  implicit def MapRead[A](implicit ra: DynamoRead[A]): DynamoRead[Map[String, A]] = DynamoRead[Map[String, A]] {
    case dynamoType@(M(e)) => sequence(e.map(a => lift(ra.read(a._2).map(r => (a._1, r))))).read(dynamoType).map(_.toMap)
    case e => DynamoReadError("", s"was expecting M got $e")
  }

}

