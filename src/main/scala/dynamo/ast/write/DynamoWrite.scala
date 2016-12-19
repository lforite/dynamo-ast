package dynamo.ast.write

import dynamo.ast._

trait DynamoWrite[A] {
  def write(a: A): DynamoType
}

object DynamoWrite extends PrimitiveWrite with CollectionWrite {
  def apply[A](implicit write: DynamoWrite[A]): DynamoWrite[A] = write
  def m(fields: (String, DynamoType)*): DynamoType = M(fields.toList)

  def at(path: String) = new WriteAt(path)
  class WriteAt(path: String) {
    def write[A](a: A)(implicit dynamoWrite: DynamoWrite[A]): (String, DynamoType) = {
      (path, dynamoWrite.write(a))
    }
  }
}

trait PrimitiveWrite {
  implicit object StringWrite extends DynamoWrite[String] {
    override def write(a: String): S = S(a)
  }

  implicit object IntWrite extends DynamoWrite[Int] {
    override def write(a: Int): N = N(a.toString)
  }

  implicit object ShortWrite extends DynamoWrite[Short] {
    override def write(a: Short): N = N(a.toString)
  }

  implicit object LongWrite extends DynamoWrite[Long] {
    override def write(a: Long): N = N(a.toString)
  }

  implicit object FloatWrite extends DynamoWrite[Float] {
    override def write(a: Float): N = N(a.toString)
  }

  implicit object DoubleWrite extends DynamoWrite[Double] {
    override def write(a: Double): N = N(a.toString)
  }

  implicit object BooleanWrite extends DynamoWrite[Boolean] {
    override def write(a: Boolean): BOOL = BOOL(a)
  }
}

trait CollectionWrite { self: PrimitiveWrite =>
  implicit def ListWrite[A](implicit ra: DynamoWrite[A]): DynamoWrite[List[A]] = (as: List[A]) => L(as.map(ra.write))
  implicit def SetWriteString: DynamoWrite[Set[String]] = (as: Set[String]) => SS(as.map(StringWrite.write))
  implicit def SetWriteInt: DynamoWrite[Set[Int]] = (as: Set[Int]) => NS(as.map(IntWrite.write))
  implicit def SetWriteShort: DynamoWrite[Set[Short]] = (as: Set[Short]) => NS(as.map(ShortWrite.write))
  implicit def SetWriteFloat: DynamoWrite[Set[Float]] = (as: Set[Float]) => NS(as.map(FloatWrite.write))
  implicit def SetWriteLong: DynamoWrite[Set[Long]] = (as: Set[Long]) => NS(as.map(LongWrite.write))
  implicit def SetWriteDouble: DynamoWrite[Set[Double]] = (as: Set[Double]) => NS(as.map(DoubleWrite.write))
  implicit def MapWrite[A](implicit ra: DynamoWrite[A]): DynamoWrite[Map[String, A]] = (a: Map[String, A]) => M(a.map(kv => (kv._1, ra.write(kv._2))).toList)
}
