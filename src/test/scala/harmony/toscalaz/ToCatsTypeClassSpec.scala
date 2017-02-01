package harmony.toscalaz

import harmony.ReversableNatTrans
import org.scalatest.FunSuite

class ToCatsTypeClassSpec extends FunSuite {

  test("InvariantFunctor value converts") {
    import harmony.toscalaz.typeclass.InvariantFunctorConverter._

    val catsI: cats.functor.Invariant[cats.data.NonEmptyList] = implicitly
    val scalazI: scalaz.InvariantFunctor[cats.data.NonEmptyList] = catsI

    assertResult(cats.data.NonEmptyList("1", List()))(scalazI.xmap[Int, String](cats.data.NonEmptyList(1, List()), _.toString, _.toInt))
  }

  test("InvariantFunctor instance converts") {
    import harmony.toscalaz.typeclass.InvariantFunctorConverter._
    import cats.data.NonEmptyList.catsDataInstancesForNonEmptyList

    assertResult(cats.data.NonEmptyList("1", List()))(scalaz.InvariantFunctor[cats.data.NonEmptyList].xmap[Int, String](cats.data.NonEmptyList(1, List()), _.toString, _.toInt))
  }

  test("InvariantFunctor instance converts2") {
    import harmony.toscalaz.typeclass.InvariantFunctorConverter._
    import harmony.toscalaz.data.NonEmptyListConverter._
    import harmony.tocats.data.NonEmptyListConverter._

    val c = cats.functor.Invariant[cats.data.NonEmptyList]
    val s: scalaz.InvariantFunctor[scalaz.NonEmptyList] = c

    assertResult(scalaz.NonEmptyList("1"))(s.xmap[Int, String](scalaz.NonEmptyList(1), _.toString, _.toInt))
  }

  test("InvariantFunctor instance and inner instance converts") {
    import harmony.toscalaz.typeclass.InvariantFunctorConverter._
    import harmony.tocats.data.NonEmptyListConverter._
    import harmony.toscalaz.data.NonEmptyListConverter._

    val c: scalaz.InvariantFunctor[scalaz.NonEmptyList] = catsToScalazInvariantFunctorInstance[cats.data.NonEmptyList, scalaz.NonEmptyList, scalaz.NonEmptyList, cats.data.NonEmptyList](cats.functor.Invariant[cats.data.NonEmptyList], ReversableNatTrans[cats.data.NonEmptyList, scalaz.NonEmptyList, scalaz.NonEmptyList, cats.data.NonEmptyList])

    assertResult(scalaz.NonEmptyList("1"))(c.xmap[Int, String](scalaz.NonEmptyList(1), _.toString, _.toInt))
  }

}
