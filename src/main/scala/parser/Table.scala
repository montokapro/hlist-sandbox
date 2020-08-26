package parser

import scala.collection.immutable.{:: => Cons}
import scala.util.{Try, Success, Failure}

import shapeless._

// https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/csv.scala

object Tables {
  case class Num(num: Int) extends AnyVal
  case class Fruit(fruit: String) extends AnyVal
  case class Four(four: String) extends AnyVal

  type NumFruit = Num :: Fruit :: HNil

  val data: List[String] = List(
    "1 banana",
    "2 banana",
    "3 apple",
    "3 banana",
    "3 cherry",
    "4 banana",
    "4 cherry"
  )

  val parsed: List[NumFruit] = List(
    Num(1) :: Fruit("banana") :: HNil,
    Num(2) :: Fruit("banana") :: HNil,
    Num(3) :: Fruit("apple") :: HNil,
    Num(3) :: Fruit("banana") :: HNil,
    Num(3) :: Fruit("cherry") :: HNil,
    Num(4) :: Fruit("banana") :: HNil,
    Num(4) :: Fruit("cherry") :: HNil
  )

  val trivial = HNil
  val trivialResult = parsed

  val three = Num(3) :: HNil
  val threeResult = List(
    Fruit("apple") :: HNil,
    Fruit("banana") :: HNil,
    Fruit("cherry") :: HNil
  )

  val threeFruit = Num(3) :: Fruit("cherry") :: HNil
  val threeFruitResult = List(HNil)

  // def refine = {
  //   Tables.parsed.flatMap(t =>
  //     if (t.head == Tables.Num(3)) {
  //       List(t.tail)
  //     } else {
  //       Nil
  //     }
  //   )
  // }

  // def refine2(l: HList) = {
  //   Tables.parsed.flatMap(t =>
  //     if (t.head == Tables.Num(3)) {
  //       refine2(List(t.tail))
  //     } else {
  //       Nil
  //     }
  //   )
  // }

  // def refine2[A](l: A :: HList, key: A) = {
  //   Tables.parsed.flatMap(t =>
  //     if (t.head == key) {
  //       refine2(List(t.tail))
  //     } else {
  //       Nil
  //     }
  //   )
  // }
}

trait Converter[T] {
  def from(s: String): Try[T]
  def to(t: T): String
}

object Converter {
  import Tables._

  def apply[T](implicit st: Lazy[Converter[T]]): Converter[T] = st.value

  def fail(s: String) = Failure(new RuntimeException(s))


  implicit def numConverter: Converter[Num] = new Converter[Num] {
    def from(a: String): Try[Num] = Try(Num(a.toInt))
    def to(a: Num): String = a.num.toString
  }

  implicit def fruitConverter: Converter[Fruit] = new Converter[Fruit] {
    def from(a: String): Try[Fruit] = Success(Fruit(a))
    def to(a: Fruit): String = a.fruit
  }

  def listCsvLinesConverter[A](l: List[String])(implicit ec: Converter[A])
      : Try[List[A]] = l match {
    case Nil => Success(Nil)
    case Cons(s,ss) => for {
        x <- ec.from(s)
        xs <- listCsvLinesConverter(ss)(ec)
      } yield Cons(x, xs)
  }

  implicit def listConverter[A](
    implicit ec: Converter[A]
  ): Converter[List[A]] = new Converter[List[A]] {
    def from(s: String): Try[List[A]] = listCsvLinesConverter(s.split("\n").toList)(ec)
    def to(l: List[A]): String = l.map(ec.to).mkString("\n")
  }


  implicit def deriveHNil: Converter[HNil] =
    new Converter[HNil] {
      def from(s: String): Try[HNil] = s match {
        case "" => Success(HNil)
        case s => fail("Cannot convert '" ++ s ++ "' to HNil")
      }
      def to(n: HNil) = ""
    }

  implicit def deriveHCons[H, T <: HList](
    implicit
    headC: Lazy[Converter[H]],
    tailC: Lazy[Converter[T]]
  ): Converter[H :: T] = new Converter[H :: T] {
    def from(s: String): Try[H :: T] = s.span(_ != ',') match {
      case (before, after) =>
        for {
          front <- headC.value.from(before)
          back <- tailC.value.from(if (after.isEmpty) after else after.tail)
        } yield front :: back

      case _ => fail("Cannot convert '" ++ s ++ "' to HList")
    }

    // scala> Converter[Tables.Num :: Tables.Num :: HNil].to(Tables.Num(1234) :: Tables.Num(5678) :: HNil)
    def to(ft: H :: T): String = {
      val head = headC.value.to(ft.head)
      val tail = ft.tail match {
        case HNil => headC.value.to(ft.head)
        case _ => headC.value.to(ft.head) ++ "," ++ tailC.value.to(ft.tail)
      }
      head ++ tail
    }
  }

  // implicit def deriveHConsOption[V, T <: HList](
  //   implicit
  //   scv: Lazy[Converter[H]],
  //   sct: Lazy[Converter[T]]
  // ): Converter[Option[H] :: T] = new Converter[Option[H] :: T] {
  //   def from(s: String): Try[Option[H] :: T] = s.span(_ != ',') match {
  //     case (before,after) =>
  //       (for {
  //         front <- scv.value.from(before)
  //         back <- sct.value.from(if (after.isEmpty) after else after.tail)
  //       } yield Some(front) :: back).orElse {
  //         sct.value.from(if (s.isEmpty) s else s.tail).map(None :: _)
  //       }

  //     case _ => fail("Cannot convert '" ++ s ++ "' to HList")
  //   }

  //   def to(ft: Option[H] :: T): String = {
  //     ft.head.map(scv.value.to(_) ++ ",").getOrElse("") ++ sct.value.to(ft.tail)
  //   }
  // }


  implicit def deriveClass[A,R](
    implicit
    gen: Generic.Aux[A,R],
    conv: Converter[R]
  ): Converter[A] = new Converter[A] {
    def from(s: String): Try[A] = conv.from(s).map(gen.from)
    def to(a: A): String = conv.to(gen.to(a))
  }
}

trait Converter2[T] {
  def from(s: String): Try[T]
  def to(t: T): String
}

object Converter2 {
  import Tables._

  def apply[T](implicit st: Lazy[Converter2[T]]): Converter2[T] = st.value

  def fail(s: String) = Failure(new RuntimeException(s))

  implicit def numConverter: Converter2[Num] = new Converter2[Num] {
    def from(a: String): Try[Num] = Try(Num(a.toInt))
    def to(a: Num): String = a.num.toString
  }

  implicit def fourConverter2: Converter2[Four] = new Converter2[Four] {
    def from(a: String): Try[Four] = Success(Four(a))
    def to(a: Four): String = a.four
  }

  implicit def deriveHNil: Converter2[HNil] =
    new Converter2[HNil] {
      def from(s: String): Try[HNil] = s match {
        case "" => Success(HNil)
        case s => fail("Cannot convert '" ++ s ++ "' to HNil")
      }
      def to(n: HNil) = ""
    }

  implicit def deriveFourCons[T <: HList](
    implicit
    headC: Lazy[Converter2[Four]],
    tailC: Lazy[Converter2[T]]
  ): Converter2[Four :: T] = new Converter2[Four :: T] {
    def from(s: String): Try[Four :: T] = s.splitAt(4) match {
      case (before, after) =>
        for {
          front <- headC.value.from(before)
          back <- tailC.value.from(if (after.isEmpty) after else after.tail)
        } yield front :: back

      case _ => fail("Cannot convert '" ++ s ++ "' to HList")
    }

    def to(ft: Four :: T): String = {
      val head = headC.value.to(ft.head)
      val tail = ft.tail match {
        case HNil => ""
        case _ => "," ++ tailC.value.to(ft.tail)
      }
      head ++ tail
    }
  }

  implicit def deriveHCons[H, T <: HList](
    implicit
    headC: Lazy[Converter2[H]],
    tailC: Lazy[Converter2[T]]
  ): Converter2[H :: T] = new Converter2[H :: T] {
    def from(s: String): Try[H :: T] = s.span(_ != ',') match {
      case (before, after) =>
        for {
          front <- headC.value.from(before)
          back <- tailC.value.from(if (after.isEmpty) after else after.tail)
        } yield front :: back

      case _ => fail("Cannot convert '" ++ s ++ "' to HList")
    }

    def to(ft: H :: T): String = {
      val head = headC.value.to(ft.head)
      val tail = ft.tail match {
        case HNil => ""
        case _ => "," ++ tailC.value.to(ft.tail)
      }
      head ++ tail
    }
  }

  // scala> Converter2[Tables.Four :: HNil].from("1234").get

  // scala> Converter2[Tables.Num :: Tables.Four :: Tables.Num :: HNil].from("0,1234,1").get.select[Tables.Four]
  // res3: parser.Tables.Four = Four(1234)

  // scala> Converter2[Tables.Four :: Tables.Four :: Tables.Num :: HNil].to(Tables.Four("1234") :: Tables.Four("5678") :: Tables.Num(1234) :: HNil)
  // res0: String = 1234,5678,1234
}
