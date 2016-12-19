package dynamo.ast

import dynamo.ast.reads.DynamoRead
import dynamo.ast.reads.DynamoRead._
import cats.implicits._


object PlayGround {
  def main(args: Array[String]): Unit = {
    val m = M(List(
      "url" -> S("the url "),
      "alt" -> S("alternative"),
      "price" -> N("12"),
      "urls" -> L(List(S("hello"), S("world"))),
      "currencies" -> SS(Set(S("hello"))),
      "prices" -> L(List(N("1.12"), N("1.12"), N("1.14"))),
      "urls2" -> L(List(M(List("test" -> N("hello"))))),
      "desc" -> M(List("title" -> S("is it working ?"))),
      "translations" -> M(List("fr_FR" -> S("The world is mine")))))
    println(Image.imageDynamoFormat.read(m))

   val test: DynamoRead[TinyImage] =
     (read("url").as[String]
        |@| read("alt").as[String]) map TinyImage.apply

    println(test.read(M(List("url" -> S("the url "),
      "alt" -> S("alternative")))))

  }
}

case class TinyImage(url: String, alt: String)
case class Product(
  id: String,
  title: String,
  tags: Set[String],
  description: String,
  images: List[Image],
  ean: String
)

case class Image(
  url: String,
  alt: String,
  urls: List[String],
  currencies: Set[String],
  prices: Set[Float],
  desc: Desc,
  price: Int,
  translations: Map[String, String]
)

object Image {

  import Desc.descReads
  import DynamoRead._

  implicit val imageDynamoFormat: DynamoRead[Image] =
    (read("url").as[String] |@|
        read[String].at("alt") |@|
        read[List[String]].at("urls") |@|
        read[Set[String]].at("currencies") |@|
        read("prices").as[Set[Float]] |@|
        read[Desc].at("desc") |@|
        read[Int].at("price") |@|
        read[Map[String, String]].at("translations")) map Image.apply
}


case class Desc(title: String)

object Desc {

  import DynamoRead._

  implicit val descReads: DynamoRead[Desc] = read[S].at("title").map(_.value).map(Desc.apply)
}


