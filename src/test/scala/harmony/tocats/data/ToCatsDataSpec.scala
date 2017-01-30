package harmony.tocats.data

import org.scalatest.FunSuite

class ToCatsDataSpec extends FunSuite {

  test("Ordering converts") {
    import harmony.tocats.data.OrderingConverter._

    assertResult(cats.kernel.Comparison.EqualTo)(scalaz.Ordering.EQ: cats.kernel.Comparison)
    assertResult(cats.kernel.Comparison.LessThan)(scalaz.Ordering.LT: cats.kernel.Comparison)
    assertResult(cats.kernel.Comparison.GreaterThan)(scalaz.Ordering.GT: cats.kernel.Comparison)
  }

  test("EitherT converts") {
    import harmony.tocats.data.EitherTConverter._
    import cats.instances.list._

    val r = scalaz.EitherT[List, String, Int](List(scalaz.DRight(1)))
    val l = scalaz.EitherT[List, String, Int](List(scalaz.DLeft("1")))

    assertResult(List(Right(1)))((r: cats.data.EitherT[List, String, Int]).value)
    assertResult(List(Left("1")))((l: cats.data.EitherT[List, String, Int]).value)
  }

  test("EitherT converts with scalaz list functor") {
    import harmony.tocats.data.EitherTConverter._
    import harmony.tocats.typeclass.FunctorConverter._
    import scalaz.std.list._

    val r = scalaz.EitherT[List, String, Int](List(scalaz.DRight(1)))
    val l = scalaz.EitherT[List, String, Int](List(scalaz.DLeft("1")))

    assertResult(List(Right(1)))((r: cats.data.EitherT[List, String, Int]).value)
    assertResult(List(Left("1")))((l: cats.data.EitherT[List, String, Int]).value)
  }

  //Name tests
  test("Name converts") {
    import harmony.tocats.data.NameConverter._

    val increments = {
      var i = 0
      scalaz.Name {
        i += 1
        i
      }
    }

    val name: cats.Always[Int] = increments

    assertResult(1)(name.value)
    assertResult(2)(name.value)
  }

  test("Now converts") {
    import harmony.tocats.data.NameConverter._

    val const = scalaz.Value(1)

    val now: cats.Now[Int] = const

    assertResult(1)(now.value)
    assertResult(1)(now.value)
  }

  test("Need converts") {
    import harmony.tocats.data.NameConverter._

    val start = System.currentTimeMillis()

    val const = scalaz.Need(System.currentTimeMillis())

    val name: cats.Always[Long] = const

    val values =
      Seq.fill(3) {
        Thread.sleep(100L)
        name.value
      }

    assert(values.head > start + 99L, "The first value is > start.")
    assert(values.tail.forall(_ == values.head), "The value was only computed once.")
  }

  test("Id converts") {

    val zeroScalaz: scalaz.Id.Id[Int] = 0
    val zeroCats: cats.Id[Int] = zeroScalaz

    assertResult(zeroCats)(0)
  }

  test("IdT converts") {
    import harmony.tocats.data.IdTConverter._

    val zeroScalaz = scalaz.IdT[List, Int](List(0))
    val zeroCats: cats.data.IdT[List, Int] = zeroScalaz

    assertResult(zeroCats.value)(List(0))
  }

  test("Kleisli[List, ?] converts") {
    import harmony.tocats.data.KleisliConverter._

    val kScalaz = scalaz.Kleisli[List, Int, String](a => List(a.toString))
    val kCats: cats.data.Kleisli[List, Int, String] = kScalaz

    assertResult(List("1"))(kCats.run(1))
  }

  test("Kleisli[NonEmptyList, ?] converts") {
    import harmony.tocats.data.KleisliConverter._
    import harmony.tocats.data.NonEmptyListConverter._

    val kScalaz = scalaz.Kleisli[scalaz.NonEmptyList, Int, String](a => scalaz.NonEmptyList(a.toString))
    val kCats: cats.data.Kleisli[cats.data.NonEmptyList, Int, String] = kScalaz

    assertResult(cats.data.NonEmptyList("1", List()))(kCats.run(1))
  }

  test("Maybe converts to Option") {
    import harmony.tocats.data.MaybeConverter._

    val scalazEmpty: scalaz.Maybe.Empty[Int] = scalaz.Maybe.Empty[Int]()
    val scalazJust: scalaz.Maybe.Just[Int] = scalaz.Maybe.Just(1)
    val scalazMaybe: scalaz.Maybe[Int] = scalaz.Maybe.Just(1)

    val scalaNone: scala.None.type = scalazEmpty
    val scalaSome: scala.Some[Int] = scalazJust
    val scalaOption: scala.Option[Int] = scalazMaybe

    assertResult(None)(scalaNone)
    assertResult(Some(1))(scalaSome)
    assertResult(Some(1))(scalaOption)
  }

  test("NonEmptyList converts") {
    import harmony.tocats.data.NonEmptyListConverter._

    val s0 = scalaz.NonEmptyList(0)
    val s1 = scalaz.NonEmptyList(0, 1, 2, 3)

    val c0: cats.data.NonEmptyList[Int] = s0
    val c1: cats.data.NonEmptyList[Int] = s1

    assertResult(cats.data.NonEmptyList(0, List.empty))(c0)
    assertResult(cats.data.NonEmptyList(0, List(1, 2, 3)))(c1)
  }

  test("OneAndStream converts") {
    import harmony.tocats.data.OneAndConverter._

    val s = scalaz.OneAnd[Stream, String]("hi", Stream("bye"))

    val c: cats.data.NonEmptyStream[String] = s

    assertResult(cats.data.NonEmptyStream[String]("hi", "bye"))(c)
  }

  test("OneAndVector converts") {
    import harmony.tocats.data.OneAndConverter._

    val s = scalaz.OneAnd[Vector, String]("hi", Vector("bye"))

    val c: cats.data.NonEmptyVector[String] = s

    assertResult(cats.data.NonEmptyVector[String]("hi", Vector("bye")))(c)
  }

  test("Validated converts") {
    import harmony.tocats.data.ValidationConverter._

    val scalazSuccess =
      scalaz.Success[Int](0)

    val scalazFailure =
      scalaz.Failure[String]("hi")

    val scalazValidation =
      scalaz.Validation.success[String, Int](0)

    val catsValid: cats.data.Validated.Valid[Int] = scalazSuccess

    val catsInvalid: cats.data.Validated.Invalid[String] = scalazFailure

    val catsValidation: cats.data.Validated[String, Int] = scalazValidation

    assertResult(cats.data.Validated.Valid(0))(catsValid)
    assertResult(cats.data.Validated.Invalid("hi"))(catsInvalid)
    assertResult(cats.data.Validated.Valid(0))(catsValidation)
  }

  test("ValidatedNel converts") {
    import harmony.tocats.data.ValidationNelConverter._

    val scalazValid =
      scalaz.Success[Int](0)

    val scalazInvalid =
      scalaz.Failure[scalaz.NonEmptyList[String]](scalaz.NonEmptyList("hi", "bye"))

    val catsValid: cats.data.ValidatedNel[String, Int] = scalazValid

    val catsInvalid: cats.data.ValidatedNel[String, Int] = scalazInvalid

    assertResult(cats.data.Validated.Valid(0))(catsValid)
    assertResult(cats.data.Validated.Invalid(cats.data.NonEmptyList("hi", List("bye"))))(catsInvalid)
  }

}
