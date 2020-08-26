package parser

import cats.collections._

// https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/csv.scala

// trait Api[A] {
//   def (a: A)

//   def before(a: A)

//   def startsWith(a: A)
// }

object Api {
  sealed trait Seek
  case class Gte(a: String) extends Seek
  case class Lt(a: String) extends Seek
  case class StartsWith(a: String) extends Seek
}

object StringInstances {
  implicit def stringDiscrete(implicit D: Discrete[Char]): Discrete[String] =
    new Discrete[String] {
      import Integral.Implicits._
      override def succ(a: String): String = a.init + D.succ(a.last)
      override def pred(a: String): String = a.init + D.pred(a.last)
    }
}
