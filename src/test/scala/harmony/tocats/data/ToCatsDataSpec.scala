package harmony.tocats.data

import org.scalatest.FunSuite

class ToCatsDataSpec extends FunSuite {

  test("example works") {
    import cats.instances.int._
    import harmony.tocats.data.WriterTConverter._

    val tell3 = scalaz.WriterT.tell(3)

    val w: cats.data.WriterT[cats.Id, Int, Unit] =
      for {
        _ <- cats.data.Writer.tell(2)
        _ <- tell3
      } yield ()

    assertResult((5, ()))(w.run)
  }

}
