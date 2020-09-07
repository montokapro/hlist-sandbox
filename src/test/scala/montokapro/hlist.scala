package montokapro

// import org.junit.Test
// import org.junit.Assert._

import ops.hlist._
import shapeless._

import org.scalatest.FunSpec

class HListSpec extends FunSpec {
  describe("startsWith") {
    it("should handle nils") {
      val startsWith0 = StartsWith[HNil, HNil]
      assert(startsWith0(HNil) == HNil)

      val startsWith1 = StartsWith[HNil, Int :: HNil]
      assert(startsWith1(HNil) == HNil)

      val startsWith2 = StartsWith[Int :: HNil, HNil]
      assert(startsWith2(1 :: HNil) == HNil)
    }

    it("should handle lists") {
      type Have = Double :: Int :: Char :: String :: HNil
      type Want = Int :: Char :: Long :: String :: HNil

      val have: Have = 1.0 :: 2 :: '3' :: "four" :: HNil

      val startsWith = StartsWith[Have, Want]
      assert(startsWith(have) == 2 :: '3' :: HNil)
    }
  }
}
