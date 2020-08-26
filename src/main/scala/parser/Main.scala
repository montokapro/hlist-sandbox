package parser

import shapeless._

case class A(a: Int)

case class B(b: String)

case class C(c: String)

case class Abc(a: A, b: B, c: C)

// scala> type H = A :: B :: C :: HNil
// defined type alias H

// scala> val h: H = A(Data.a) :: B(Data.b) :: C(Data.c) :: HNil
// h: H = A(1234) :: B(five) :: C(6789) :: HNil

// To HList and back again
// scala> Generic[Abc].from(Generic[Abc].to(Abc(A(Data.a), B(Data.b), C(Data.c))))
// res17: parser.Abc = Abc(A(1234),B(five),C(6789))

trait Encoder[A] {
  def encode(value: A): String
}

object Encoder {
  // "Summoner" method
  def apply[A](implicit e: Encoder[A]): Encoder[A] = e

  // "Constructor" method
  def instance[A](f: A => String): Encoder[A] =
    new Encoder[A] {
      def encode(value: A): String = f(value)
    }

  // Globally visible type class instances
  implicit val aEncoder: Encoder[A] =
    instance(_.a.toString)

  implicit val bEncoder: Encoder[B] =
    instance(_.b)

  implicit val cEncoder: Encoder[C] =
    instance(_.c)

  implicit val hnilEncoder: Encoder[HNil] =
    instance(hnil => "")

  // implicit def hconsEncoder[H, T <: HList](
  //   implicit
  //   headEncoder: Lazy[Encoder[H]],
  //   tailEncoder: Lazy[Encoder[T]]
  // ): Encoder[H :: T] = instance {
  //   case head :: tail =>
  //     headEncoder.value.encode(head) ++ "," ++ tailEncoder.value.encode(tail)
  // }

  implicit def hconsEncoder[H, T <: HList](
    implicit
    headEncoder: Encoder[H],
    tailEncoder: Encoder[T]
  ): Encoder[H :: T] = instance {
    case head :: tail =>
      headEncoder.encode(head) ++ "," ++ tailEncoder.encode(tail)
  }

  // Encoder.encode(A(1) :: B("test") :: HNil)
  def encode[A](a: A)(implicit e: Encoder[A]) = e.encode(a)
}

// implicit val encoder: Encoder[Abc] =
//   new Encoder[Abc] {
//     def encode(abc: Abc): String =
//       s"${abc.a},${abc.b},${abc.c}"
//   }


object Parser {
  type Error = String

  trait PartialParser[T] {
    def apply(s: String): Either[Error, (T, String)]
  }

  // trait TotalParser[T] {
  //   def apply(s: String): Either[Error, T]
  // }

  // AParser extends PartialParser[A] {
  //   def apply(s: String): Either[Error, (A, String)] = ???
  // }

//   ListParser[H <: HList] {
//     def apply(s: String): HList
//   }
}

object Data {
  val example = "1234,five-6789"
  val a = 1234
  val b = "five"
  val c = "6789"

  type H = A :: B :: C :: HNil
}

object Main extends App {

}
