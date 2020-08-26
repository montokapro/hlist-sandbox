package potree

import org.scalatest.FunSpec
import cats.kernel.Eq
import cats.kernel.Order
import cats.kernel.PartialOrder

// This ordering is a sort of 'hash' that hints at a partial ordering
class TreeOrderSpec extends FunSpec {
  class EqPartialOrder[A](eq: Eq[A]) extends PartialOrder[A] {
    def partialCompare(x: A, y: A): Double =
      if (eq.eqv(x, y)) 0.0 else Double.NaN
  }

  describe("compare") {
    import cats.instances.int._

    val treeOInstances = new TreeOrderInstances(Order[Int])

    def leaf(a: Int): Tree[Int] = Leaf(a)
    def or(as: Tree[Int]*): Tree[Int] = Or(as.toList)
    def and(as: Tree[Int]*): Tree[Int] = And(as.toList)

    implicit val treeO = treeOInstances.treeO

    import cats.kernel.Comparison._
    import cats.syntax.order._

    val po = new EqPartialOrder(Eq[Int])
    val treePoInstances = new TreePartialOrderInstances(po)
    val treePo = treePoInstances.treePo

    describe("leaf") {
      it("should compare equal") {
        assert(treePo.partialComparison(leaf(1), leaf(1)) == Some(EqualTo))
        assert(treeO.partialComparison(leaf(1), leaf(1)) == Some(EqualTo))

        assert(treePo.eqv(leaf(1), leaf(1)))
        assert(treeO.eqv(leaf(1), leaf(1)))
      }

      // This test shows how the total order acts as an "upper bound"
      // to the partial order
      it("should compare unequal") {
        assert(treePo.partialComparison(leaf(1), leaf(2)) != Some(EqualTo))
        assert(treeO.partialComparison(leaf(1), leaf(2)) != Some(EqualTo))
        assert(treeO.partialComparison(leaf(1), leaf(2)) == Some(LessThan))

        assert(treePo.neqv(leaf(1), leaf(2)))
        assert(treeO.neqv(leaf(1), leaf(2)))
        assert(leaf(1) < leaf(2))
      }

      it("should compare inverse") {
        assert(treePo.partialComparison(leaf(-1), leaf(1)) != Some(EqualTo))
        assert(treeO.partialComparison(leaf(-1), leaf(1)) != Some(EqualTo))
        assert(treeO.partialComparison(leaf(-1), leaf(1)) == Some(LessThan))

        assert(treePo.neqv(leaf(-1), leaf(1)))
        assert(treeO.neqv(leaf(-1), leaf(1)))
        assert(leaf(-1) < leaf(1))
      }
    }

    describe("or") {
      it("should compare empty") {
        val less = or(
        )
        val more = or(
          leaf(1)
        )
        assert(more > less)
      }

      it("should compare shallow") {
        val less = or(
          leaf(1),
          leaf(2)
        )
        val more = or(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(more > less)
      }

      it("should compare leaf") {
        val less = leaf(1)
        val more = or(
          leaf(1),
          leaf(2)
        )
        assert(more > less)
      }

      it("should compare nested") {
        val less = or(
          leaf(1),
          and(
            leaf(2),
            leaf(3)
          )
        )
        val more = or(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(more > less)
      }

      it("should compare complex") {
        val less = or(
          leaf(1),
          and(
            leaf(2),
            leaf(3),
            leaf(4)
          )
        )
        val more = or(
          leaf(1),
          leaf(2),
          and(
            leaf(3),
            leaf(4)
          )
        )
        assert(more > less)
      }
    }

    describe("and") {
      it("should compare empty") {
        val less = and(
        )
        val more = and(
          leaf(1)
        )
        assert(more > less)
      }

      it("should compare shallow") {
        val less = and(
          leaf(1),
          leaf(2)
        )
        val more = and(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(more > less)
      }

      it("should compare leaf") {
        val less = leaf(1)
        val more = and(
          leaf(1),
          leaf(2)
        )
        assert(more > less)
      }

      it("should compare nested") {
        val less = and(
          leaf(1),
          or(
            leaf(2),
            leaf(3)
          )
        )
        val more = and(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(more > less)
      }

      it("should compare complex") {
        val less = and(
          leaf(1),
          or(
            leaf(2),
            leaf(3),
            leaf(4)
          )
        )
        val more = and(
          leaf(1),
          leaf(2),
          or(
            leaf(3),
            leaf(4)
          )
        )
        assert(more > less)
      }
    }

    describe("tree") {
      it("should compare ors and ands") {
        assert(or(leaf(1), leaf(2)) < and(leaf(1), leaf(2)))
      }

      it("should compare zero and one") {
        assert(or() < and())
      }
    }
  }

  describe("compare") {
    import cats.instances.int._

    val treeOInstances = new TreeOrderInstances(Order[Int])

    def leaf(a: Int): Tree[Int] = Leaf(a)
    def or(as: Tree[Int]*): Tree[Int] = Or(as.toList)
    def and(as: Tree[Int]*): Tree[Int] = And(as.toList)

    implicit val treeO = treeOInstances.treeO

    import cats.kernel.Comparison._
    import cats.syntax.order._

    val po = new EqPartialOrder(Eq[Int])
    val treePoInstances = new TreePartialOrderInstances(po)
    val treePo = treePoInstances.treePo

    describe("leaf") {
      it("should compare equal") {
        assert(treePo.partialComparison(leaf(1), leaf(1)) == Some(EqualTo))
        assert(treeO.partialComparison(leaf(1), leaf(1)) == Some(EqualTo))

        assert(treePo.eqv(leaf(1), leaf(1)))
        assert(treeO.eqv(leaf(1), leaf(1)))
      }

      // This test shows how the total order acts as an "upper bound"
      // to the partial order
      it("should compare unequal") {
        assert(treePo.partialComparison(leaf(1), leaf(2)) != Some(EqualTo))
        assert(treeO.partialComparison(leaf(1), leaf(2)) != Some(EqualTo))
        assert(treeO.partialComparison(leaf(1), leaf(2)) == Some(LessThan))

        assert(treePo.neqv(leaf(1), leaf(2)))
        assert(treeO.neqv(leaf(1), leaf(2)))
        assert(leaf(1) < leaf(2))
      }

      it("should compare inverse") {
        assert(treePo.partialComparison(leaf(-1), leaf(1)) != Some(EqualTo))
        assert(treeO.partialComparison(leaf(-1), leaf(1)) != Some(EqualTo))
        assert(treeO.partialComparison(leaf(-1), leaf(1)) == Some(LessThan))

        assert(treePo.neqv(leaf(-1), leaf(1)))
        assert(treeO.neqv(leaf(-1), leaf(1)))
        assert(leaf(-1) < leaf(1))
      }
    }
  }
}
