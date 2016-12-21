package dynamo.ast.reads

import cats._
import cats.implicits._
import dynamo.ast._
import dynamo.ast.reads.DynamoRead._

trait DynamoRead[A] { self =>
  def read(dynamoType: DynamoType): DynamoReadResult[A]
}

object DynamoRead extends DefaultReads with PrimitiveRead with CollectionRead {

  implicit val applicative: Applicative[DynamoRead] = new Applicative[DynamoRead] {
    override def pure[A](x: A): DynamoRead[A] = new DynamoRead[A] {
      override def read(dynamoType: DynamoType): DynamoReadResult[A] = DynamoReadSuccess(x)
    }

    override def ap[A, B](ff: DynamoRead[(A) => B])(fa: DynamoRead[A]): DynamoRead[B] = new DynamoRead[B] {
      override def read(dynamoType: DynamoType): DynamoReadResult[B] = fa.read(dynamoType) match {
        case DynamoReadSuccess(a) => ff.read(dynamoType).map((aToB) => aToB(a))
        case e: DynamoReadError => e
      }
    }
  }

  def lift[A](a: DynamoReadResult[A]): DynamoRead[A] = new DynamoRead[A] {
    override def read(dynamoType: DynamoType): DynamoReadResult[A] = a
  }

  def apply[A](implicit read: DynamoRead[A]): DynamoRead[A] = read

  def read[A]: ReadAt[A] = new ReadAt[A]

  class ReadAt[A] {
    def at(path: String)(implicit reads: DynamoRead[A]): DynamoRead[A] = new DynamoRead[A] {
      override def read(dynamoType: DynamoType): DynamoReadResult[A] = {
        MRead.read(dynamoType).flatMap { m: M =>
          m.elements.find(_._1 == path).map(_._2).fold[DynamoReadResult[A]](DynamoReadError(path, "Path not found"))(me => reads.read(me).withPath(path))
        }
      }
    }
  }

  def read(path: String) = new ReadAs(path)

  class ReadAs(at: String) {
    def as[A](implicit reads: DynamoRead[A]): DynamoRead[A] = new DynamoRead[A] {
      override def read(dynamoType: DynamoType): DynamoReadResult[A] = MRead.read(dynamoType).flatMap { m: M =>
        m.elements.find(_._1 == at).map(_._2).fold[DynamoReadResult[A]](DynamoReadError(at, "Path not found"))(me => reads.read(me).withPath(at))
      }
    }
  }

  def readOpt[A]: ReadOptAt[A] = new ReadOptAt[A]

  class ReadOptAt[A] {
    def at(path: String)(implicit reads: DynamoRead[A]): DynamoRead[Option[A]] = new DynamoRead[Option[A]] {
      override def read(dynamoType: DynamoType): DynamoReadResult[Option[A]] =  {
        MRead.read(dynamoType).flatMap { m: M =>
          m.elements.find(_._1 == path).map(_._2).fold[DynamoReadResult[Option[A]]](DynamoReadSuccess(None))(me => reads.read(me).map(e => Some(e)).withPath(path))
        }
      }
    }
  }

  def readOpt(path: String) = new ReadOptAs(path)

  class ReadOptAs(at: String) {
    def as[A](implicit reads: DynamoRead[A]): DynamoRead[Option[A]] =  new DynamoRead[Option[A]] {
      override def read(dynamoType: DynamoType): DynamoReadResult[Option[A]] = {
        MRead.read(dynamoType).flatMap { m: M =>
          m.elements.find(_._1 == at).map(_._2).fold[DynamoReadResult[Option[A]]](DynamoReadSuccess(None))(me => reads.read(me).map(e => Some(e)).withPath(at))
        }
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

  implicit def LRead[A <: DynamoType](implicit ra: DynamoRead[A]): DynamoRead[L[A]] = new DynamoRead[L[A]]{
    override def read(dynamoType: DynamoType): DynamoReadResult[L[A]] = dynamoType match {
      case dynamoType@(L(e)) => e.map(a => lift(ra.read(a))).sequence[DynamoRead, A].read(dynamoType).map(L.apply)
      case e => DynamoReadError("", s"was expecting L got $e")
    }
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

trait PrimitiveRead {

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

trait CollectionRead {
  self: PrimitiveRead =>
  implicit def ListRead[A](implicit ra: DynamoRead[A]): DynamoRead[List[A]] = new DynamoRead[List[A]] {
    override def read(dynamoType: DynamoType): DynamoReadResult[List[A]] = dynamoType match {
      case dynamoType@(l@L(e)) => e.map(a => lift(ra.read(a))).sequence[DynamoRead, A].read(dynamoType)
      case e => DynamoReadError("", s"was expecting L got $e")
    }
  }

  implicit def SetRead[A](implicit ra: DynamoRead[A]): DynamoRead[Set[A]] = new DynamoRead[Set[A]] {
    override def read(dynamoType: DynamoType): DynamoReadResult[Set[A]] = dynamoType match {
      case dynamoType@(SS(e)) => e.map(a => lift(ra.read(a))).toList.sequence[DynamoRead, A].map(_.toSet).read(dynamoType)
      case dynamoType@(NS(e)) => e.map(a => lift(ra.read(a))).toList.sequence[DynamoRead, A].map(_.toSet).read(dynamoType)
      case dynamoType@(L(e)) => e.map(a => lift(ra.read(a))).sequence[DynamoRead, A].map(_.toSet).read(dynamoType)
      case e => DynamoReadError("", s"was expecting SS, NS or L got $e")
    }
  }

  implicit def MapRead[A](implicit ra: DynamoRead[A]): DynamoRead[Map[String, A]] = new DynamoRead[Map[String, A]] {
    override def read(dynamoType: DynamoType): DynamoReadResult[Map[String, A]] =  dynamoType match {
      case M(e) => e.map(a => lift(ra.read(a._2).map(r => (a._1, r)))).sequence[DynamoRead, (String, A)].read(dynamoType).map(_.toMap)
      case e => DynamoReadError("", s"was expecting M got $e")
    }
  }

}

