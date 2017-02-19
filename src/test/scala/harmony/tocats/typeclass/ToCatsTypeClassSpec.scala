package harmony.tocats.typeclass

import org.scalatest.FunSuite

class ToCatsTypeClassSpec extends FunSuite {

  test("NonEmptyList example works") {
    import harmony.tocats.typeclass.FunctorConverter._

    val x = scalaz.NonEmptyList(0, 1, 2, 3)

    val f = scalaz.Functor[scalaz.NonEmptyList]

    val catsF: cats.Functor[scalaz.NonEmptyList] = f

    val result = catsF.map(x)(_.toString)

    assertResult(scalaz.NonEmptyList[String]("0", "1", "2", "3"))(result)
  }

}
