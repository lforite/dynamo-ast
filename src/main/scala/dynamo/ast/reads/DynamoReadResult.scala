package dynamo.ast.reads

sealed trait DynamoReadResult[+A] {
  def map[B](f: A => B): DynamoReadResult[B] = this match {
    case DynamoReadSuccess(a) => DynamoReadSuccess(f(a))
    case e: DynamoReadError => e
  }

  def flatMap[B](f: A => DynamoReadResult[B]): DynamoReadResult[B] = this match {
    case DynamoReadSuccess(a) => f(a)
    case e: DynamoReadError => e
  }

  def withPath(path: String): DynamoReadResult[A] = this match {
    case e: DynamoReadError => e.copy(path = path)
    case s => s
  }
}

case class DynamoReadSuccess[A](a: A) extends DynamoReadResult[A]
case class DynamoReadError(path: String, error: String) extends DynamoReadResult[Nothing]
