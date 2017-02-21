package harmony.toscalaz.data

import cats.Eval
import harmony.{NaturalTransformation, BiNaturalTransformation}
import scalaz.{Maybe, Name}

trait ComparisonConverter {
  implicit def catsToScalazOrderingEQ(inner: cats.kernel.Comparison.EqualTo.type): scalaz.Ordering.EQ.type =
    scalaz.Ordering.EQ

  implicit def catsToScalazOrderingGT(inner: cats.kernel.Comparison.GreaterThan.type): scalaz.Ordering.GT.type =
    scalaz.Ordering.GT

  implicit def catsToScalazOrderingLT(inner: cats.kernel.Comparison.LessThan.type): scalaz.Ordering.LT.type =
    scalaz.Ordering.LT

  implicit def catsToScalazOrdering[F](inner: cats.kernel.Comparison): scalaz.Ordering =
    scalaz.Ordering.fromInt(inner.toInt)
}

object ComparisonConverter extends ComparisonConverter

trait CoproductConverter {
  implicit def catsToScalazCoproductNaturalTransformation[F[_], G[_], F0[_], G0[_]](implicit F: NaturalTransformation[F, F0], G: NaturalTransformation[G, G0]): NaturalTransformation[cats.data.Coproduct[F, G, ?], scalaz.Coproduct[F0, G0, ?]] =
    new NaturalTransformation[cats.data.Coproduct[F, G, ?], scalaz.Coproduct[F0, G0, ?]] {
      override def apply[A](fa: cats.data.Coproduct[F, G, A]): scalaz.Coproduct[F0, G0, A] =
        scalaz.Coproduct[F0, G0, A](fa.run.fold((a: F[A]) => scalaz.DLeft(F.apply(a)), (a: G[A]) => scalaz.DRight(G.apply(a))))
    }

  implicit def catsToScalazCoproductValue[F[_], G[_], F0[_], G0[_], A](inner: cats.data.Coproduct[F, G, A])(implicit F: NaturalTransformation[F, F0], G: NaturalTransformation[G, G0]): scalaz.Coproduct[F0, G0, A] =
    catsToScalazCoproductNaturalTransformation[F, G, F0, G0].apply[A](inner)
}

object CoproductConverter extends CoproductConverter

trait CoyonedaConverter {
  implicit def catsToScalazCoyonedaNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0]): NaturalTransformation[cats.free.Coyoneda[F, ?], scalaz.Coyoneda[F0, ?]] =
    new NaturalTransformation[cats.free.Coyoneda[F, ?], scalaz.Coyoneda[F0, ?]] {
      override def apply[A](fa: cats.free.Coyoneda[F, A]): scalaz.Coyoneda[F0, A] =
        scalaz.Coyoneda[F0, fa.Pivot, A](trans.apply(fa.fi))(fa.k)
    }

  implicit def catsToScalazCoyoneda[F[_], F0[_], A](inner: cats.free.Coyoneda[F, A])(implicit trans: NaturalTransformation[F, F0]): scalaz.Coyoneda[F0, A] =
    catsToScalazCoyonedaNaturalTransformation[F, F0].apply[A](inner)
}

object CoyonedaConverter extends CoyonedaConverter

trait EitherConverter {
  implicit val catsToScalazDisjunctionNaturalTransformation: BiNaturalTransformation[scala.Either, scalaz.Disjunction] =
    new BiNaturalTransformation[scala.Either, scalaz.Disjunction] {
      override def apply[A, B](f: scala.Either[A, B]): scalaz.Disjunction[A, B] =
        scalaz.Disjunction.fromEither(f)
    }

  implicit def scalaLeftToDLeft[A, B](s: scala.Left[A, B]): scalaz.DLeft[A] =
    scalaz.DLeft(s.left.get)

  implicit def scalaRightToDRight[A, B](s: scala.Right[A, B]): scalaz.DRight[B] =
    scalaz.DRight(s.right.get)

  implicit def scalaEitherToDisjunction[A, B](s: scala.Either[A, B]): scalaz.Disjunction[A, B] =
    catsToScalazDisjunctionNaturalTransformation.apply[A, B](s)
}

object EitherConverter extends EitherConverter

trait EitherTConverter {
  implicit def catsToScalazEitherTBiNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0], F0: scalaz.Functor[F0]): BiNaturalTransformation[cats.data.EitherT[F, ?, ?], scalaz.EitherT[F0, ?, ?]] =
    new BiNaturalTransformation[cats.data.EitherT[F, ?, ?], scalaz.EitherT[F0, ?, ?]] {
      override def apply[A, B](f: cats.data.EitherT[F, A, B]): scalaz.EitherT[F0, A, B] =
        scalaz.EitherT.fromEither[F0, A, B](trans.apply(f.value))
    }

  implicit def catsToScalazEitherT[F[_], F0[_], A, B](inner: cats.data.EitherT[F, A, B])(implicit trans: NaturalTransformation[F, F0], F: scalaz.Functor[F0]): scalaz.EitherT[F0, A, B] =
    catsToScalazEitherTBiNaturalTransformation[F, F0].apply[A, B](inner)
}

object EitherTConverter extends EitherTConverter

trait EvalConverter {
  implicit val catsToScalazNameNaturalTransformation: NaturalTransformation[cats.Eval, scalaz.Name] =
    new NaturalTransformation[cats.Eval, scalaz.Name] {
      override def apply[A](fa: Eval[A]): Name[A] =
        fa match {
          case always: cats.Always[A] =>
            catsToScalazName(always)
          case now@cats.Now(_) =>
            catsToScalazValue(now)
          case later: cats.Later[A] =>
            catsLaterToScalazNeed(later)
          case call: cats.Eval.Call[A] =>
            catsEvalCallToScalazNeed(call)
          case compute: cats.Eval.Compute[A] =>
            catsEvalComputeToScalazNeed(compute)
        }
    }

  implicit def catsToScalazName[A](inner: cats.Always[A]): scalaz.Name[A] =
    scalaz.Name(inner.value)

  implicit def catsToScalazValue[A](inner: cats.Now[A]): scalaz.Value[A] =
    scalaz.Value(inner.value)

  implicit def catsLaterToScalazNeed[A](inner: cats.Later[A]): scalaz.Need[A] =
    scalaz.Need(inner.value)

  implicit def catsEvalCallToScalazNeed[A](inner: cats.Eval.Call[A]): scalaz.Need[A] =
    scalaz.Need(inner.value)

  implicit def catsEvalComputeToScalazNeed[A](inner: cats.Eval.Compute[A]): scalaz.Need[A] =
    scalaz.Need(inner.value)

  implicit def catsToScalazName[A](inner: cats.Eval[A]): scalaz.Name[A] =
    catsToScalazNameNaturalTransformation.apply(inner)
}

object EvalConverter extends EvalConverter

trait IdTConverter {
  implicit def catsToScalazIdTNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0]): NaturalTransformation[cats.data.IdT[F, ?], scalaz.IdT[F0, ?]] =
    new NaturalTransformation[cats.data.IdT[F, ?], scalaz.IdT[F0, ?]] {
      override def apply[A](fa: cats.data.IdT[F, A]): scalaz.IdT[F0, A] =
        scalaz.IdT(trans.apply(fa.value))
    }

  implicit def catsToScalazIdT[F[_], F0[_], A](inner: cats.data.IdT[F, A])(implicit trans: NaturalTransformation[F, F0]): scalaz.IdT[F0, A] =
    catsToScalazIdTNaturalTransformation[F, F0].apply[A](inner)
}

object IdTConverter extends IdTConverter

trait KleisliConverter {
  implicit def catsToScalazKleisliBiNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0]): BiNaturalTransformation[cats.data.Kleisli[F, ?, ?], scalaz.Kleisli[F0, ?, ?]] =
    new BiNaturalTransformation[cats.data.Kleisli[F, ?, ?], scalaz.Kleisli[F0, ?, ?]] {
      override def apply[A, B](f: cats.data.Kleisli[F, A, B]): scalaz.Kleisli[F0, A, B] =
        scalaz.Kleisli(a => trans.apply(f.run(a)))
    }

  implicit def catsToScalazKleisli[F[_], F0[_], A, B](inner: cats.data.Kleisli[F, A, B])(implicit trans: NaturalTransformation[F, F0]): scalaz.Kleisli[F0, A, B] =
    catsToScalazKleisliBiNaturalTransformation[F, F0].apply[A, B](inner)

}

object KleisliConverter extends KleisliConverter

trait NonEmptyListConverter {
  implicit val catsToScalazNonEmptyListNaturalTransformation: NaturalTransformation[cats.data.NonEmptyList, scalaz.NonEmptyList] =
    new NaturalTransformation[cats.data.NonEmptyList, scalaz.NonEmptyList] {
      override def apply[A](fa: cats.data.NonEmptyList[A]): scalaz.NonEmptyList[A] =
        scalaz.NonEmptyList[A](fa.head, fa.tail: _*)
    }

  implicit def catsToScalazNonEmptyList[A](inner: cats.data.NonEmptyList[A]): scalaz.NonEmptyList[A] =
    catsToScalazNonEmptyListNaturalTransformation.apply(inner)
}

object NonEmptyListConverter extends NonEmptyListConverter

trait NonEmptyVectorConverter {
  implicit val catsToScalazOneAndVectorNaturalTransformation: NaturalTransformation[cats.data.NonEmptyVector, scalaz.OneAnd[Vector, ?]] =
    new NaturalTransformation[cats.data.NonEmptyVector, scalaz.OneAnd[Vector, ?]] {
      override def apply[A](fa: cats.data.NonEmptyVector[A]): scalaz.OneAnd[Vector, A] =
        scalaz.OneAnd[Vector, A](fa.head, fa.tail)
    }

  implicit def catsNonEmptyVectorToScalaz[A](inner: cats.data.NonEmptyVector[A]): scalaz.OneAnd[Vector, A] =
    catsToScalazOneAndVectorNaturalTransformation.apply[A](inner)
}

object NonEmptyVectorConverter extends NonEmptyVectorConverter

trait OneAndConverter {
  implicit def catsToScalazOneAndListNaturalTransformation[F[_], F0[_]](implicit inner: NaturalTransformation[F, F0]): NaturalTransformation[cats.data.OneAnd[F, ?], scalaz.OneAnd[F0, ?]] =
    new NaturalTransformation[cats.data.OneAnd[F, ?], scalaz.OneAnd[F0, ?]] {
      override def apply[A](fa: cats.data.OneAnd[F, A]): scalaz.OneAnd[F0, A] =
        scalaz.OneAnd(fa.head, inner.apply(fa.tail))
    }

  implicit def catsToScalazOneAnd[F[_], F0[_], A](a: cats.data.OneAnd[F, A])(implicit inner: NaturalTransformation[F, F0]): scalaz.OneAnd[F0, A] =
    catsToScalazOneAndListNaturalTransformation[F, F0].apply[A](a)

  //Overrides for concrete type cats.data.NonEmptyVector. Remove these if cats.data.NonEmptyVector becomes an alias for cats.data.OneAnd[Vector, ?].
  implicit val catsNonEmptyVectorToScalazNaturalTransformation: NaturalTransformation[cats.data.NonEmptyVector, scalaz.OneAnd[Vector, ?]] =
    new NaturalTransformation[cats.data.NonEmptyVector, scalaz.OneAnd[Vector, ?]] {
      override def apply[A](fa: cats.data.NonEmptyVector[A]): scalaz.OneAnd[Vector, A] =
        scalaz.OneAnd(fa.head, fa.tail)
    }
}

object OneAndConverter extends OneAndConverter

trait OptionToMaybeConverter {
  implicit val catsToScalazMaybeNaturalTransformation: NaturalTransformation[scala.Option, scalaz.Maybe] =
    new NaturalTransformation[scala.Option, scalaz.Maybe] {
      override def apply[A](fa: Option[A]): Maybe[A] =
        scalaz.Maybe.fromOption(fa)
    }

  implicit def catsToScalazMaybeEmpty[A](inner: scala.None.type): scalaz.Maybe.Empty[A] =
    scalaz.Maybe.Empty[A]()

  implicit def catsToScalazMaybeJust[A](inner: scala.Some[A]): scalaz.Maybe.Just[A] =
    scalaz.Maybe.Just(inner.get)

  implicit def catsToScalazMaybe[A](inner: scala.Option[A]): scalaz.Maybe[A] =
    catsToScalazMaybeNaturalTransformation.apply[A](inner)
}

object OptionToMaybeConverter extends OptionToMaybeConverter

trait OptionTToMaybeTConverter {
  import OptionToMaybeConverter._

  implicit def catsToScalazMaybeTNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0], F0: scalaz.Functor[F0]): NaturalTransformation[cats.data.OptionT[F, ?], scalaz.MaybeT[F0, ?]] =
    new NaturalTransformation[cats.data.OptionT[F, ?], scalaz.MaybeT[F0, ?]] {
      override def apply[A](fa: cats.data.OptionT[F, A]): scalaz.MaybeT[F0, A] =
        scalaz.MaybeT(F0.map(trans(fa.value))(catsToScalazMaybe))
    }

  implicit def catsToScalazMaybeT[F[_], F0[_], A](inner: cats.data.OptionT[F, A])(implicit trans: NaturalTransformation[F, F0], F0: scalaz.Functor[F0]): scalaz.MaybeT[F0, A] =
    catsToScalazMaybeTNaturalTransformation[F, F0].apply[A](inner)
}

object OptionTToMaybeTConverter extends OptionTToMaybeTConverter

trait OptionTToOptionTConverter {
  implicit def catsToScalazOptionTNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0]): NaturalTransformation[cats.data.OptionT[F, ?], scalaz.OptionT[F0, ?]] =
    new NaturalTransformation[cats.data.OptionT[F, ?], scalaz.OptionT[F0, ?]] {
      override def apply[A](fa: cats.data.OptionT[F, A]): scalaz.OptionT[F0, A] =
        scalaz.OptionT(trans(fa.value))
    }

  implicit def catsToScalazOptionT[F[_], F0[_], A](inner: cats.data.OptionT[F, A])(implicit trans: NaturalTransformation[F, F0]): scalaz.OptionT[F0, A] =
    catsToScalazOptionTNaturalTransformation[F, F0].apply[A](inner)
}

object OptionTToOptionTConverter extends OptionTToOptionTConverter

trait StateTConverter {
  implicit def catsToScalazStateTBiNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0], F: cats.FlatMap[F], F0: scalaz.Monad[F0]): BiNaturalTransformation[cats.data.StateT[F, ?, ?], scalaz.StateT[F0, ?, ?]] =
    new BiNaturalTransformation[cats.data.StateT[F, ?, ?], scalaz.StateT[F0, ?, ?]] {
      override def apply[A, B](f: cats.data.StateT[F, A, B]): scalaz.StateT[F0, A, B] =
        scalaz.StateT[F0, A, B](a => trans(f.run(a)))
    }

  implicit def catsToScalazStateT[F[_], F0[_], A, B](inner: cats.data.StateT[F, A, B])(implicit trans: NaturalTransformation[F, F0], F: cats.FlatMap[F], F0: scalaz.Monad[F0]): scalaz.StateT[F0, A, B] =
    catsToScalazStateTBiNaturalTransformation[F, F0].apply[A, B](inner)
}

object StateTConverter extends StateTConverter

trait ValidatedConverter {
  implicit val catsToScalazValidationBiNaturalTransformation: BiNaturalTransformation[cats.data.Validated[?, ?], scalaz.Validation[?, ?]] =
    new BiNaturalTransformation[cats.data.Validated[?, ?], scalaz.Validation[?, ?]] {
      override def apply[A, B](f: cats.data.Validated[A, B]): scalaz.Validation[A, B] =
        f.fold(scalaz.Validation.failure, scalaz.Validation.success)
    }

  implicit def catsInvalidToScalazFailure[E](inner: cats.data.Validated.Invalid[E]): scalaz.Failure[E] =
    scalaz.Failure(inner.e)

  implicit def catsValidToScalazSuccess[A](inner: cats.data.Validated.Valid[A]): scalaz.Success[A] =
    scalaz.Success(inner.a)

  implicit def catsToScalazValidation[E, A](inner: cats.data.Validated[E, A]): scalaz.Validation[E, A] =
    catsToScalazValidationBiNaturalTransformation.apply[E, A](inner)
}

object ValidatedConverter extends ValidatedConverter

trait ValidatedNelConverter {
  import ValidatedConverter._
  import NonEmptyListConverter._

  implicit val catsToscalazValidationNelBiNaturalTransformation: BiNaturalTransformation[cats.data.ValidatedNel[?, ?], scalaz.ValidationNel[?, ?]] =
    new BiNaturalTransformation[cats.data.ValidatedNel[?, ?], scalaz.ValidationNel[?, ?]] {
      override def apply[A, B](f: cats.data.ValidatedNel[A, B]): scalaz.ValidationNel[A, B] =
        f.leftMap(catsToScalazNonEmptyList)
    }


  implicit def catsToScalazValidationNel[E, A](inner: cats.data.ValidatedNel[E, A]): scalaz.ValidationNel[E, A] =
    catsToscalazValidationNelBiNaturalTransformation.apply[E, A](inner)
}

object ValidatedNelConverter extends ValidatedNelConverter

trait WriterTConverter {
  implicit def catsToScalazWriterTBiNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0]): BiNaturalTransformation[cats.data.WriterT[F, ?, ?], scalaz.WriterT[F0, ?, ?]] =
    new BiNaturalTransformation[cats.data.WriterT[F, ?, ?], scalaz.WriterT[F0, ?, ?]] {
      override def apply[A, B](f: cats.data.WriterT[F, A, B]): scalaz.WriterT[F0, A, B] =
        scalaz.WriterT[F0, A, B](trans.apply(f.run))
    }

  implicit def catsToScalazWriterT[F[_], F0[_], L, V](inner: cats.data.WriterT[F, L, V])(implicit trans: NaturalTransformation[F, F0]): scalaz.WriterT[F0, L, V] =
    catsToScalazWriterTBiNaturalTransformation.apply[L, V](inner)
}

object WriterTConverter extends WriterTConverter

trait YonedaConverter {
  implicit def catsToScalazYonedaNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0], F: scalaz.Functor[F0]): NaturalTransformation[cats.free.Yoneda[F, ?], scalaz.Yoneda[F0, ?]] =
    new NaturalTransformation[cats.free.Yoneda[F, ?], scalaz.Yoneda[F0, ?]] {
      override def apply[A](fa: cats.free.Yoneda[F, A]): scalaz.Yoneda[F0, A] =
        scalaz.Yoneda[F0, A](trans.apply(fa.run))
    }

  implicit def catsToScalazYoneda[F[_], F0[_], A](inner: cats.free.Yoneda[F, A])(implicit trans: NaturalTransformation[F, F0], F: scalaz.Functor[F0]): scalaz.Yoneda[F0, A] =
    catsToScalazYonedaNaturalTransformation[F, F0].apply[A](inner)
}

object YonedaConverter extends YonedaConverter
