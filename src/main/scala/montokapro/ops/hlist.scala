package montokapro
package ops

import shapeless._

object hlist {

  /**
   * Type class supporting extracting from this `HList` as many elements as
   * possible that match a sorted list until an element is not found.
   */
  trait StartsWith[Have <: HList, Want <: HList] extends DepFn1[Have] with Serializable { type Out <: HList }

  trait LowestPriorityStartsWith {
    type Aux[Have <: HList, Want <: HList, Get <: HList] = StartsWith[Have, Want] { type Out = Get }

    implicit def hnilStartsWith[Have <: HList, Want <: HList]: Aux[Have, Want, HNil] =
      new StartsWith[Have, Want] {
        type Out = HNil
        def apply(have: Have): Out = HNil
      }
  }

  trait LowPriorityStartsWith extends LowestPriorityStartsWith {
    implicit def hlistStartsWith0[H, HaveT <: HList, Want <: HList, Get <: HList]
      (implicit startsWith: StartsWith.Aux[HaveT, Want, Get]): StartsWith.Aux[H :: HaveT, Want, Get]  =
        new StartsWith[H :: HaveT, Want] {
          type Out = Get
          def apply(have: H :: HaveT): Out = startsWith(have.tail)
        }
  }

  object StartsWith extends LowPriorityStartsWith {
    def apply[Have <: HList, Want <: HList](implicit startsWith: StartsWith[Have, Want]): Aux[Have, Want, startsWith.Out] = startsWith

    implicit def hlistStartsWith1[H, HaveT <: HList, WantT <: HList, GetT <: HList]
      (implicit startsWith: StartsWith.Aux[HaveT, WantT, GetT]): StartsWith.Aux[H :: HaveT, H :: WantT, H :: GetT]  =
        new StartsWith[H :: HaveT, H :: WantT] {
          type Out = H :: GetT
          def apply(have: H :: HaveT): Out = have.head :: startsWith(have.tail)
        }
  }
}
