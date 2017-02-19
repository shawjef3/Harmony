package harmony.tocats.typeclass

import org.scalatest.FunSuite

class ToCatsTypeClassSpec extends FunSuite {

  test("NonEmptyVector example works") {
    import harmony.tocats.typeclass.FunctorConverter._
    import scalaz.std.vector._

    val x = scalaz.OneAnd[Vector, Int](0, Vector(1, 2, 3))

    val f = scalaz.Functor[scalaz.OneAnd[Vector, ?]]

    val catsF: cats.Functor[scalaz.OneAnd[Vector, ?]] = f

    val result = catsF.map(x)(_.toString)

    assertResult(scalaz.OneAnd[Vector, String]("0", Vector("1", "2", "3")))(result)
  }

}
