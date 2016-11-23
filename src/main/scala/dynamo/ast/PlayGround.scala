package dynamo.ast

import dynamo.ast.reads.DynamoRead


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
  }
}

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
    (for {
      url <- read("url").as[String]
      alt <- read[String].at("alt")
      urls <- read[List[String]].at("urls")
      currencies <- read[Set[String]].at("currencies")
      prices <- read("prices").as[Set[Float]]
      desc <- read[Desc].at("desc")
      price <- read[Int].at("price")
      translations <- read[Map[String, String]].at("translations")
    } yield (url, alt, urls, currencies, prices, desc, price, translations)) map (Image.apply _).tupled
}


case class Desc(title: String)

object Desc {

  import DynamoRead._

  implicit val descReads: DynamoRead[Desc] = read[S].at("title").map(_.value).map(Desc.apply)
}


