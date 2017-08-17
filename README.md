# Dynamo-ast
[![Build Status](https://travis-ci.org/lforite/dynamo-ast.png?branch=master)](https://travis-ci.org/louis-forite/dynamo-ast)

A tiny library to encode DynamoDB AST. Defines types classes to read/write from/to DynamoDb types. Deeply inspired by [play-json](https://github.com/playframework/play-json) design.

Installation
-----------------------

```scala
libraryDependencies += "com.github.louis-forite" %% "dynamo-ast" % "0.1"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"
```

Minimal working example
-----------------------

```scala

import cats.implicits._
import dynamo.ast.reads.DynamoRead
import dynamo.ast.reads.DynamoRead.read
import dynamo.ast.writes.DynamoWrite
import dynamo.ast.writes.DynamoWrite._

case class Student(id: String, firstName: String, lastName: Option[String])

object Student {
  implicit val studentRead: DynamoRead[Student] =
    (read[String].at("id") |@|
      read[String].at("firstName") |@|
      readOpt[String].at("lastName")) map Student.apply

   implicit val studentWrite: DynamoWrite[Student] =
    (write[String].at("id") |@|
        write[String].at("firstName") |@|
        writeOpt[String].at("lastName")) contramap(s => (s.id, s.firstName, s.lastName))
}

val dynamoStudent = M(List(
      "id" -> S("id-1"),
      "firstName" -> S("John"),
      "lastName" -> S("Doe")))

DynamoRead[Student].read(dynamoStudent) //will yield DynamoReadSuccess(Student("id-1", "John", Some("Doe")))

val student = Student("id-1", "John", Some("Doe"))
DynamoWrite[Student].write(student) //will yield M(List("id" -> S("id-1"), "firstName" -> S("John"), "lastName" -> S("Doe")))
```

License
-------

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
