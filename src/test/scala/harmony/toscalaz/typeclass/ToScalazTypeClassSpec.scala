package harmony.toscalaz.typeclass

import org.scalatest.FunSuite

class ToScalazTypeClassSpec extends FunSuite {

  test("InvariantFunctor value converts") {
    import harmony.toscalaz.typeclass.InvariantFunctorConverter._

    val catsI: cats.functor.Invariant[cats.data.NonEmptyList] = implicitly
    val scalazI: scalaz.InvariantFunctor[cats.data.NonEmptyList] = catsI

    assertResult(cats.data.NonEmptyList("1", Nil))(scalazI.xmap[Int, String](cats.data.NonEmptyList(1, Nil), _.toString, _.toInt))
  }

  test("InvariantFunctor instance converts") {
    import cats.data.NonEmptyList.catsDataInstancesForNonEmptyList
    import harmony.toscalaz.typeclass.InvariantFunctorConverter._

    assertResult(cats.data.NonEmptyList("1", Nil))(scalaz.InvariantFunctor[cats.data.NonEmptyList].xmap[Int, String](cats.data.NonEmptyList(1, Nil), _.toString, _.toInt))
  }

  test("InvariantFunctor syntax converts") {
    import cats.data.NonEmptyList.catsDataInstancesForNonEmptyList
    import harmony.toscalaz.typeclass.InvariantFunctorConverter._
    import scalaz.Scalaz._

    assertResult(cats.data.NonEmptyList("1", Nil))(cats.data.NonEmptyList(1, Nil).xmap[String](_.toString, _.toInt))
  }

}
