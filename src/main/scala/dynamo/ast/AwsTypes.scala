package dynamo.ast

sealed trait DynamoType

sealed trait DynamoScalarType extends DynamoType

/**
  * An attribute of type String
  */
case class S(value: String) extends DynamoScalarType

/**
  * An attribute of type Number
  */
case class N(value: String) extends DynamoScalarType

/**
  * An attribute of type Boolean
  */
case class BOOL(value: Boolean) extends DynamoScalarType

/**
  * An attribute of type List
  */
case class L[A <: DynamoType](elements: List[A]) extends DynamoType

/**
  * An attribute of type Map
  */
case class M(elements: List[(String, DynamoType)]) extends DynamoType

/**
  * An attribute of type Number
  */
case class NS(numbers: Set[N]) extends DynamoType

/**
  * An attribute of type String Set
  */
case class SS(strings: Set[S]) extends DynamoType

/**
  * An attribute of type Null
  */
case object NULL extends DynamoType
