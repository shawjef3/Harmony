package harmony.tocats.typeclass

import org.scalatest.FunSuite
import scalaz.NonEmptyList

class ToCatsTypeClassSpec extends FunSuite {

  test("NonEmptyList example works") {
    import harmony.Everyone._

    val x = scalaz.NonEmptyList(0, 1, 2, 3)
    val result = cats.Functor[NonEmptyList].map(x)(_.toString)

    assertResult(scalaz.NonEmptyList[String]("0", "1", "2", "3"))(result)
  }

}
