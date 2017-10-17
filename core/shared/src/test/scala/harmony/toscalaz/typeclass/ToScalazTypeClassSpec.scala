//package harmony.toscalaz.typeclass
//
//import org.scalacheck.Gen
//import org.scalacheck.Prop.forAll
//
////adapted from scalaz's tests
//object ToScalazTypeClassSpec extends scalaz.SpecLite {
//
//  //Applicative
//  {
//    import EqConverter._
//    import MonadConverter._
//    import ShowConverter._
//
//    import cats.instances.all._
//
//    import scalaz.syntax.std.list._
//    import scalaz.syntax.applicative._
//    import scalaz.ApplicativeTest
//
//    "replicateM is the same" ! forAll { (fa: Option[Int]) =>
//      forAll(Gen.choose(0, 100)) { n =>
//        fa.replicateM(n) must_=== ApplicativeTest.replicateM(n, fa)
//      }
//    }
//
//    "filterM is the same" ! forAll { (l: List[Int]) =>
//      // don't make `None` too likely
//      def pred(n: Int) = if (n < 0 && n % 2 == 0) None else Some(n % 2 == 0)
//
//      l.filterM(pred) must_=== ApplicativeTest.filterM(l, pred)
//    }
//  }
//
//  //Apply
//  {
//    import ApplyConverter._
//    import EqConverter._
//    import MonoidConverter._
//    import ShowConverter._
//
//    import cats.instances.all._
//
//    import scalaz.syntax.apply._
//    import scalaz.std.option.some
//    import scalaz.scalacheck.ScalazProperties.applicative
//    import scalaz.scalacheck.ScalazArbitrary._
//    import scalaz.\/
//
//    checkAll("List applyApplicative", {
//      implicit val F = cats.Apply[List].applyApplicative
//      applicative.laws[λ[α => List[α] \/ α]]
//    })
//
//    "mapN" in {
//      val A: scalaz.Apply[Option] = cats.Apply[Option]
//      A.apply2(some("1"), some("2"))(_ + _) must_===(some("12"))
//      A.apply3(some("1"), some("2"), some("3"))(_ + _ + _) must_===(some("123"))
//      A.apply4(some("1"), some("2"), some("3"), some("4"))(_ + _ + _ + _) must_===(some("1234"))
//      A.apply5(some("1"), some("2"), some("3"), some("4"), some("5"))(_ + _ + _ + _ + _) must_===(some("12345"))
//
//      val S = cats.Monoid[String].applicative
//      def undefined = sys.error("")
//
//      S.ap(fa = "1")(f = "2") must_===("21")
//      S.ap2("1", "2")("3") must_===("312")
//      S.ap3("1", "2", "3")("4") must_===("4123")
//      S.ap4("1", "2", "3", "4")("5") must_===("51234")
//      S.ap5("1", "2", "3", "4", "5")("6") must_===("612345")
//
//      S.apply2("1", "2")((a, b) => undefined) must_===("12")
//      S.apply3("1", "2", "3")((a, b, c) => undefined) must_===("123")
//      S.apply4("1", "2", "3", "4")((a, b, c, d) => undefined) must_===("1234")
//      S.apply5("1", "2", "3", "4", "5")((a, b, c, d, e) => undefined) must_===("12345")
//    }
//
//    "apN" in {
//      val A: scalaz.Apply[Option] = cats.Apply[Option]
//      A.ap2(some("1"), some("2"))(some((_: String) + (_: String))) must_===(some("12"))
//      A.ap3(some("1"), some("2"), some("3"))(some((_: String) + (_: String) + (_: String))) must_===(some("123"))
//      A.ap4(some("1"), some("2"), some("3"), some("4"))(some((_: String) + (_: String) + (_: String) + (_: String))) must_===(some("1234"))
//      A.ap5(some("1"), some("2"), some("3"), some("4"), some("5"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("12345"))
//    }
//
//    "<*>" in {
//      some(9) <*> some({(_: Int) + 3}) must_===(some(12))
//    }
//  }
//
//}
