package harmony.tocats.typeclass

import harmony.ReversableNatTrans
import org.scalatest.FunSuite

class ToCatsTypeClassSpec extends FunSuite {

  test("NonEmptyVector example works") {
    import harmony.tocats.typeclass.FunctorConverter._
//    import scalaz.std.vector._
    import scalaz.Scalaz._

    val x = scalaz.OneAnd[Vector, Int](0, Vector(1, 2, 3))

    val result = cats.Functor[scalaz.OneAnd[Vector, ?]](scalazToCatsFunctor(scalaz.Functor[scalaz.OneAnd[Vector, ?]](scalaz.OneAnd.oneAndFunctor(vectorInstance)), ReversableNatTrans.refl[scalaz.OneAnd[Vector, ?]])).map(x)(_.toString)

    assertResult(scalaz.OneAnd[Vector, String]("0", Vector("1", "2", "3")))(result)
  }

}
