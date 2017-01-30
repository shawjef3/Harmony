package harmony.toscalaz.data

import org.scalatest.FunSuite

class ToScalazDataSpec extends FunSuite {

  test("Comparison converts") {
    import harmony.toscalaz.data.ComparisonConverter._

    assertResult(scalaz.Ordering.EQ)(cats.kernel.Comparison.EqualTo: scalaz.Ordering)
    assertResult(scalaz.Ordering.LT)(cats.kernel.Comparison.LessThan: scalaz.Ordering)
    assertResult(scalaz.Ordering.GT)(cats.kernel.Comparison.GreaterThan: scalaz.Ordering)
  }

  test("EitherT converts") {
    import harmony.toscalaz.data.EitherTConverter._
    import scalaz.std.list._

    val r = cats.data.EitherT[List, String, Int](List(Right(1)))
    val l = cats.data.EitherT[List, String, Int](List(Left("1")))

    assertResult(List(scalaz.DRight(1)))((r: scalaz.EitherT[List, String, Int]).run)
    assertResult(List(scalaz.DLeft("1")))((l: scalaz.EitherT[List, String, Int]).run)
  }

  //Eval tests
  test("Always converts") {
    import harmony.toscalaz.data.EvalConverter._

    val increments = {
      var i = 0
      cats.Always {
        i += 1
        i
      }
    }

    val name: scalaz.Name[Int] = increments

    assertResult(1)(name.value)
    assertResult(2)(name.value)
  }

  test("Now converts") {
    import harmony.toscalaz.data.EvalConverter._

    val const = cats.Now(1)

    val name: scalaz.Name[Int] = const

    assertResult(1)(name.value)
    assertResult(1)(name.value)
  }

  test("Later converts") {
    import harmony.toscalaz.data.EvalConverter._

    val start = System.currentTimeMillis()

    val later = cats.Later(System.currentTimeMillis())

    val name: scalaz.Name[Long] = later

    val values =
      Seq.fill(3) {
        Thread.sleep(100L)
        name.value
      }

    assert(values.head > start + 99L, "The first value is > start.")
    assert(values.tail.forall(_ == values.head), "The value was only computed once.")
  }

  test("Id converts") {

    val zeroCats: cats.Id[Int] = 0
    val zeroScalaz: scalaz.Id.Id[Int] = zeroCats

    assertResult(zeroScalaz)(0)
  }

  test("IdT converts") {
    import harmony.toscalaz.data.IdTConverter._

    val zeroCats = cats.data.IdT[List, Int](List(0))
    val zeroScalaz: scalaz.IdT[List, Int] = zeroCats

    assertResult(zeroScalaz.run)(List(0))
  }

  test("Kleisli[List, ?] converts") {
    import harmony.toscalaz.data.KleisliConverter._

    val kCats = cats.data.Kleisli[List, Int, String](a => List(a.toString))
    val kScalaz: scalaz.Kleisli[List, Int, String] = kCats

    assertResult(List("1"))(kScalaz.run(1))
  }

  test("Kleisli[NonEmptyList, ?] converts") {
    import harmony.toscalaz.data.KleisliConverter._
    import harmony.toscalaz.data.NonEmptyListConverter._

    val kCats = cats.data.Kleisli[cats.data.NonEmptyList, Int, String](a => cats.data.NonEmptyList(a.toString, List.empty))
    val kScalaz: scalaz.Kleisli[scalaz.NonEmptyList, Int, String] = kCats

    assertResult(scalaz.NonEmptyList("1"))(kScalaz.run(1))
  }

  test("NonEmptyList converts") {
    import harmony.toscalaz.data.NonEmptyListConverter._

    val c0 = cats.data.NonEmptyList(0, List.empty)
    val c1 = cats.data.NonEmptyList(0, List(1, 2, 3))

    val s0: scalaz.NonEmptyList[Int] = c0
    val s1: scalaz.NonEmptyList[Int] = c1

    assertResult(scalaz.NonEmptyList(0))(s0)
    assertResult(scalaz.NonEmptyList(0, 1, 2, 3))(s1)
  }

  test("Option converts to Maybe") {
    import harmony.toscalaz.data.OptionToMaybeConverter._

    val oNone: scala.None.type = None
    val oSome: scala.Some[Int] = Some(1)
    val oOption: scala.Option[Int] = Some(1)

    val scalazEmpty: scalaz.Maybe[Int] = oNone
    val scalazJust: scalaz.Maybe[Int] = oSome
    val scalazMaybe: scalaz.Maybe[Int] = oOption

    assertResult(scalaz.Maybe.Empty())(scalazEmpty)
    assertResult(scalaz.Maybe.Just(1))(scalazJust)
    assertResult(scalaz.Maybe.Just(1))(scalazMaybe)
  }

  test("NonEmptyStream converts") {
    import harmony.toscalaz.data.OneAndConverter._

    val c = cats.data.NonEmptyStream("hi", "bye")

    val s: scalaz.OneAnd[Stream, String] = c

    assertResult(scalaz.OneAnd("hi", Stream("bye")))(s)
  }

  test("NonEmptyVector converts") {
    import harmony.toscalaz.data.NonEmptyVectorConverter._

    val c = cats.data.NonEmptyVector("hi", Vector("bye"))

    val s: scalaz.OneAnd[Vector, String] = c

    assertResult(scalaz.OneAnd("hi", Vector("bye")))(s)
  }

  test("Validated converts") {
    import harmony.toscalaz.data.ValidatedConverter._

    val catsValid =
      cats.data.Validated.Valid[Int](0)

    val catsInvalid =
      cats.data.Validated.Invalid[String]("hi")

    val catsValidation =
      cats.data.Validated.valid[String, Int](0)

    val scalazSuccess: scalaz.Success[Int] = catsValid

    val scalazFailure: scalaz.Failure[String] = catsInvalid

    val scalazValidation: scalaz.Validation[String, Int] = catsValidation

    assertResult(scalaz.Validation.success(0))(scalazSuccess)
    assertResult(scalaz.Validation.failure("hi"))(scalazFailure)
    assertResult(scalaz.Validation.success(0))(scalazValidation)
  }

  test("ValidatedNel.Valid converts") {
    import harmony.toscalaz.data.ValidatedNelConverter._

    val catsValid: cats.data.ValidatedNel[String, Int] =
      cats.data.Validated.Valid(0)

    val scalazValid: scalaz.ValidationNel[String, Int] = catsValid

    assertResult(scalaz.Validation.success(0))(scalazValid)
  }

  test("ValidatedNel.Invalid converts") {
    import harmony.toscalaz.data.ValidatedNelConverter._

    val catsInvalid: cats.data.ValidatedNel[String, Int] =
      cats.data.Validated.Invalid(cats.data.NonEmptyList("hi", List("bye")))

    val scalazInvalid: scalaz.ValidationNel[String, Int] = catsInvalid

    assertResult(scalaz.Validation.failure(scalaz.NonEmptyList("hi", "bye")))(scalazInvalid)
  }

}
