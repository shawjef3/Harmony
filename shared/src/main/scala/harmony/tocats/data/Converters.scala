package harmony.tocats.data

import cats.Eval
import harmony.{NaturalTransformation, BiNaturalTransformation}
import scalaz.Name

trait CoproductConverter {
  implicit def scalazToCatsCoproductNaturalTransformation[F[_], G[_], F0[_], G0[_]](implicit F: NaturalTransformation[F, F0], G: NaturalTransformation[G, G0]): NaturalTransformation[scalaz.Coproduct[F, G, ?], cats.data.Coproduct[F0, G0, ?]] =
    new NaturalTransformation[scalaz.Coproduct[F, G, ?], cats.data.Coproduct[F0, G0, ?]] {
      override def apply[A](fa: scalaz.Coproduct[F, G, A]): cats.data.Coproduct[F0, G0, A] =
        cats.data.Coproduct[F0, G0, A](fa.run.bimap(F.apply, G.apply).toEither)
    }

  implicit def scalazToCatsCoproduct[F[_], G[_], F0[_], G0[_], A](inner: scalaz.Coproduct[F, G, A])(implicit F: NaturalTransformation[F, F0], G: NaturalTransformation[G, G0]): cats.data.Coproduct[F0, G0, A] =
    scalazToCatsCoproductNaturalTransformation[F, G, F0, G0].apply[A](inner)

}

object CoproductConverter extends CoproductConverter

trait CoyonedaConverter {

  implicit def scalazToCatsCoyonedaNaturalTransformation[F[_], F0[_]](implicit inner: NaturalTransformation[F, F0]): NaturalTransformation[scalaz.Coyoneda[F, ?], cats.free.Coyoneda[F0, ?]] =
    new NaturalTransformation[scalaz.Coyoneda[F, ?], cats.free.Coyoneda[F0, ?]] {
      override def apply[A](fa: scalaz.Coyoneda[F, A]): cats.free.Coyoneda[F0, A] =
        cats.free.Coyoneda[F0, fa.I, A](inner.apply(fa.fi))(fa.k)
    }

  implicit def scalazToCatsCoyonedaValue[F[_], F0[_], A](c: scalaz.Coyoneda[F, A])(implicit F: scalaz.Functor[F], inner: NaturalTransformation[F, F0]): cats.free.Coyoneda[F0, A] =
    scalazToCatsCoyonedaNaturalTransformation[F, F0].apply[A](c)

}

object CoyonedaConverter extends CoyonedaConverter

trait DisjunctionConverter {
  implicit val scalazDisjunctionToScalaEitherNaturalTransformation: BiNaturalTransformation[scalaz.Disjunction, scala.Either] =
    new BiNaturalTransformation[scalaz.Disjunction, scala.Either] {
      override def apply[A, B](f: scalaz.Disjunction[A, B]): Either[A, B] =
        f.toEither
    }

  implicit def dLeftToScalaLeft[A, B](s: scalaz.DLeft[A]): scala.Left[A, B] =
    scala.Left(s.a)

  implicit def dRightToScalaRight[A, B](s: scalaz.DRight[B]): scala.Right[A, B] =
    scala.Right(s.b)

  implicit def disjunctionToScalaEither[A, B](s: scalaz.Disjunction[A, B]): scala.Either[A, B] =
    scalazDisjunctionToScalaEitherNaturalTransformation.apply[A, B](s)
}

object DisjunctionConverter extends DisjunctionConverter

trait EitherTConverter {
  import harmony.toscalaz.typeclass.FunctorConverter._

  implicit def scalazToCatsEitherTBiNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0], F: cats.Functor[F]): BiNaturalTransformation[scalaz.EitherT[F, ?, ?], cats.data.EitherT[F0, ?, ?]] =
    new BiNaturalTransformation[scalaz.EitherT[F, ?, ?], cats.data.EitherT[F0, ?, ?]] {
      override def apply[A, B](f: scalaz.EitherT[F, A, B]): cats.data.EitherT[F0, A, B] =
        cats.data.EitherT[F0, A, B](trans.apply(f.toEither))
    }

  implicit def scalazToCatsEitherTValue[F[_], F0[_], A, B](inner: scalaz.EitherT[F, A, B])(implicit trans: NaturalTransformation[F, F0], F: cats.Functor[F]): cats.data.EitherT[F0, A, B] =
    scalazToCatsEitherTBiNaturalTransformation.apply(inner)

}

object EitherTConverter extends EitherTConverter

trait IdTConverter {
  implicit def scalazToCatsIdTNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0]): NaturalTransformation[scalaz.IdT[F, ?], cats.data.IdT[F0, ?]] =
    new NaturalTransformation[scalaz.IdT[F, ?], cats.data.IdT[F0, ?]] {
      override def apply[A](fa: scalaz.IdT[F, A]): cats.data.IdT[F0, A] =
        cats.data.IdT(trans.apply(fa.run))
    }

  implicit def scalazToCatsIdT[F[_], F0[_], A](inner: scalaz.IdT[F, A])(implicit trans: NaturalTransformation[F, F0]): cats.data.IdT[F0, A] =
    scalazToCatsIdTNaturalTransformation[F, F0].apply[A](inner)
}

object IdTConverter extends IdTConverter

trait KleisliConverter {
  implicit def scalazToCatsKleisliBiNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0]): BiNaturalTransformation[scalaz.Kleisli[F, ?, ?], cats.data.Kleisli[F0, ?, ?]] =
    new BiNaturalTransformation[scalaz.Kleisli[F, ?, ?], cats.data.Kleisli[F0, ?, ?]] {
      override def apply[A, B](f: scalaz.Kleisli[F, A, B]): cats.data.Kleisli[F0, A, B] =
        cats.data.Kleisli(a => trans.apply(f.run(a)))
    }

  implicit def scalazToCatsKleisli[F[_], F0[_], A, B](inner: scalaz.Kleisli[F, A, B])(implicit trans: NaturalTransformation[F, F0]): cats.data.Kleisli[F0, A, B] =
    scalazToCatsKleisliBiNaturalTransformation[F, F0].apply[A, B](inner)
}

object KleisliConverter extends KleisliConverter

trait MaybeConverter {
  implicit val scalazMaybeToScalaOptionNaturalTransoformation: NaturalTransformation[scalaz.Maybe, scala.Option] =
    new NaturalTransformation[scalaz.Maybe, scala.Option] {
      override def apply[A](fa: scalaz.Maybe[A]): Option[A] =
        fa.toOption
    }

  implicit def scalazMaybeEmptyToCats[A](inner: scalaz.Maybe.Empty[A]): scala.None.type =
    scala.None

  implicit def scalazMaybeJustToCats[A](inner: scalaz.Maybe.Just[A]): scala.Some[A] =
    scala.Some(inner.a)

  implicit def scalazMaybeToCats[A](inner: scalaz.Maybe[A]): scala.Option[A] =
    scalazMaybeToScalaOptionNaturalTransoformation.apply[A](inner)
}

object MaybeConverter extends MaybeConverter

trait MaybeTConverter {
  import MaybeConverter._

  implicit def scalazMaybeTToCatsOptionTNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0], F: cats.Functor[F]): NaturalTransformation[scalaz.MaybeT[F, ?], cats.data.OptionT[F0, ?]] =
    new NaturalTransformation[scalaz.MaybeT[F, ?], cats.data.OptionT[F0, ?]] {
      override def apply[A](fa: scalaz.MaybeT[F, A]): cats.data.OptionT[F0, A] =
        cats.data.OptionT[F0, A](trans.apply(F.map(fa.run)(scalazMaybeToCats)))
    }

  implicit def scalazMaybeTToCatsOptionT[F[_], F0[_], A](inner: scalaz.MaybeT[F, A])(implicit trans: NaturalTransformation[F, F0], F: cats.Functor[F]): cats.data.OptionT[F0, A] =
    scalazMaybeTToCatsOptionTNaturalTransformation[F, F0].apply[A](inner)
}

object MaybeTConverter extends MaybeTConverter

trait NameConverter {
  implicit val scalazToCatsEvalNaturalTransformation: NaturalTransformation[scalaz.Name, cats.Eval] =
    new NaturalTransformation[scalaz.Name, cats.Eval] {
      override def apply[A](fa: Name[A]): Eval[A] =
        fa match {
          case v: scalaz.Value[A] =>
            scalazToCatsNow(v)
          case need: scalaz.Need[A] =>
            scalazToCatsLater(need)
          case name: scalaz.Name[A] =>
            scalazToCatsAlways(name)
        }
    }

  implicit def scalazToCatsNow[A](inner: scalaz.Value[A]): cats.Now[A] =
    cats.Now(inner.value)

  implicit def scalazToCatsLater[A](inner: scalaz.Need[A]): cats.Later[A] =
    cats.Later(inner.value)

  implicit def scalazToCatsAlways[A](inner: scalaz.Name[A]): cats.Always[A] =
    cats.Always(inner.value)

  implicit def scalazToCatsEval[A](inner: scalaz.Name[A]): cats.Eval[A] =
    scalazToCatsEvalNaturalTransformation.apply(inner)
}

object NameConverter extends NameConverter

trait NonEmptyListConverter {
  implicit lazy val scalazToCatsNonEmptyListNaturalTransformation: NaturalTransformation[scalaz.NonEmptyList, cats.data.NonEmptyList] =
    new NaturalTransformation[scalaz.NonEmptyList, cats.data.NonEmptyList] {
      override def apply[A](fa: scalaz.NonEmptyList[A]): cats.data.NonEmptyList[A] =
        cats.data.NonEmptyList[A](fa.head, fa.tail.toList)
    }

  implicit def scalazToCatsNonEmptyList[A](inner: scalaz.NonEmptyList[A]): cats.data.NonEmptyList[A] =
    scalazToCatsNonEmptyListNaturalTransformation.apply(inner)
}

object NonEmptyListConverter extends NonEmptyListConverter

trait IListConverter {
  implicit val scalazIListToScalaListNaturalTransformation: NaturalTransformation[scalaz.IList, scala.List] =
    new NaturalTransformation[scalaz.IList, scala.List] {
      override def apply[A](fa: scalaz.IList[A]): scala.List[A] =
        fa.toList
    }

  implicit def scalazIListToScalaList[A](inner: scalaz.IList[A]): scala.List[A] =
    scalazIListToScalaListNaturalTransformation.apply(inner)
}

object IListConverter extends IListConverter

trait OneAndConverter {
  implicit def scalazToCatsOneAndNaturalTransformation[F[_], F0[_]](implicit inner: NaturalTransformation[F, F0]): NaturalTransformation[scalaz.OneAnd[F, ?], cats.data.OneAnd[F0, ?]] =
    new NaturalTransformation[scalaz.OneAnd[F, ?], cats.data.OneAnd[F0, ?]] {
      override def apply[A](fa: scalaz.OneAnd[F, A]): cats.data.OneAnd[F0, A] =
        cats.data.OneAnd(fa.head, inner.apply(fa.tail))
    }

  implicit def scalazToCatsOneAnd[F[_], F0[_], A](a: scalaz.OneAnd[F, A])(implicit inner: NaturalTransformation[F, F0]): cats.data.OneAnd[F0, A] =
    scalazToCatsOneAndNaturalTransformation[F, F0].apply(a)

  //Overrides for concrete type cats.data.NonEmptyVector. Remove these if cats.data.NonEmptyVector becomes an alias for cats.data.OneAnd[Vector, ?].
  implicit val scalazToCatsNonEmptyVectorNaturalTransformation: NaturalTransformation[scalaz.OneAnd[Vector, ?], cats.data.NonEmptyVector] =
    new NaturalTransformation[scalaz.OneAnd[Vector, ?], cats.data.NonEmptyVector] {
      override def apply[A](fa: scalaz.OneAnd[Vector, A]): cats.data.NonEmptyVector[A] =
        cats.data.NonEmptyVector(fa.head, fa.tail)
    }

  implicit def scalazToCatsNonEmptyVector[A](inner: scalaz.OneAnd[Vector, A]): cats.data.NonEmptyVector[A] =
    scalazToCatsNonEmptyVectorNaturalTransformation.apply[A](inner)
}

object OneAndConverter extends OneAndConverter

trait OptionTConverter {
  implicit def scalazOptionTToCatsOptionTNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0]): NaturalTransformation[scalaz.OptionT[F, ?], cats.data.OptionT[F0, ?]] =
    new NaturalTransformation[scalaz.OptionT[F, ?], cats.data.OptionT[F0, ?]] {
      override def apply[A](fa: scalaz.OptionT[F, A]): cats.data.OptionT[F0, A] =
        cats.data.OptionT[F0, A](trans.apply(fa.run))
    }

  implicit def scalazOptionTToCatsOptionT[F[_], F0[_], A](inner: scalaz.OptionT[F, A])(implicit trans: NaturalTransformation[F, F0]): cats.data.OptionT[F0, A] =
    scalazOptionTToCatsOptionTNaturalTransformation[F, F0].apply[A](inner)
}

object OptionTConverter extends OptionTConverter

trait OrderingConverter {
  implicit def scalazToCatsOrderingEqualTo(inner: scalaz.Ordering.EQ.type): cats.kernel.Comparison.EqualTo.type =
    cats.kernel.Comparison.EqualTo

  implicit def scalazToCatsOrderingGreaterThan(inner: scalaz.Ordering.GT.type): cats.kernel.Comparison.GreaterThan.type =
    cats.kernel.Comparison.GreaterThan

  implicit def scalazToCatsOrderingLessThan(inner: scalaz.Ordering.LT.type): cats.kernel.Comparison.LessThan.type =
    cats.kernel.Comparison.LessThan

  implicit def scalazToCatsOrdering[F](inner: scalaz.Ordering): cats.kernel.Comparison =
    cats.kernel.Comparison.fromInt(inner.toInt)
}

object OrderingConverter extends OrderingConverter

trait StateTConverter {
  implicit def scalazToCatsStateTBiNaturalTransformation[F[_], F0[_]](implicit F: scalaz.Monad[F], A: cats.Applicative[F0], trans: NaturalTransformation[F, F0]): BiNaturalTransformation[scalaz.StateT[F, ?, ?], cats.data.StateT[F0, ?, ?]] =
    new BiNaturalTransformation[scalaz.StateT[F, ?, ?], cats.data.StateT[F0, ?, ?]] {
      override def apply[A, B](f: scalaz.StateT[F, A, B]): cats.data.StateT[F0, A, B] =
        cats.data.StateT[F0, A, B](a => trans.apply(f.run(a)))
    }


  implicit def scalazToCatsStateT[F[_], F0[_], L, V](inner: scalaz.StateT[F, L, V])(implicit F: scalaz.Monad[F], A: cats.Applicative[F0], trans: NaturalTransformation[F, F0]): cats.data.StateT[F0, L, V] =
    scalazToCatsStateTBiNaturalTransformation[F, F0].apply[L, V](inner)
}

object StateTConverter extends StateTConverter

trait ValidationConverter {
  implicit def scalazToCatsInvalid[E](inner: scalaz.Failure[E]): cats.data.Validated.Invalid[E] =
    cats.data.Validated.Invalid(inner.e)

  implicit def scalazToCatsValid[A](inner: scalaz.Success[A]): cats.data.Validated.Valid[A] =
    cats.data.Validated.Valid(inner.a)

  implicit def scalazToCatsValidated[E, A](inner: scalaz.Validation[E, A]): cats.data.Validated[E, A] =
    inner.fold(cats.data.Validated.invalid, cats.data.Validated.valid)
}

object ValidationConverter extends ValidationConverter

trait ValidationNelConverter {
  import ValidationConverter._
  import NonEmptyListConverter._

  implicit val scalazToCatsValidatedNelBiNaturalTransformation: BiNaturalTransformation[scalaz.ValidationNel[?, ?], cats.data.ValidatedNel[?, ?]] =
    new BiNaturalTransformation[scalaz.ValidationNel[?, ?], cats.data.ValidatedNel[?, ?]] {
      override def apply[A, B](f: scalaz.ValidationNel[A, B]): cats.data.ValidatedNel[A, B] =
        f.leftMap(scalazToCatsNonEmptyList)
    }

  implicit def scalazToCatsValidatedNel[E, A](inner: scalaz.ValidationNel[E, A]): cats.data.ValidatedNel[E, A] =
    scalazToCatsValidatedNelBiNaturalTransformation.apply[E, A](inner)
}

object ValidationNelConverter extends ValidationNelConverter

trait WriterTConverter {
  implicit def scalazToCatsWriterTBiNaturalTransformation[F[_], F0[_]](implicit trans: NaturalTransformation[F, F0]): BiNaturalTransformation[scalaz.WriterT[F, ?, ?], cats.data.WriterT[F0, ?, ?]] =
    new BiNaturalTransformation[scalaz.WriterT[F, ?, ?], cats.data.WriterT[F0, ?, ?]] {
      override def apply[A, B](f: scalaz.WriterT[F, A, B]): cats.data.WriterT[F0, A, B] =
        cats.data.WriterT[F0, A, B](trans.apply(f.run))
    }

  implicit def scalazToCatsWriterT[F[_], F0[_], L, V](inner: scalaz.WriterT[F, L, V])(implicit trans: NaturalTransformation[F, F0]): cats.data.WriterT[F0, L, V] =
    scalazToCatsWriterTBiNaturalTransformation.apply[L, V](inner)
}

object WriterTConverter extends WriterTConverter

trait YonedaConverter {
  implicit def scalazToCatsYonedaNaturalTransformation[F[_], F0[_]](implicit inner: NaturalTransformation[F, F0], F: cats.Functor[F0]): NaturalTransformation[scalaz.Yoneda[F, ?], cats.free.Yoneda[F0, ?]] =
    new NaturalTransformation[scalaz.Yoneda[F, ?], cats.free.Yoneda[F0, ?]] {
      override def apply[A](fa: scalaz.Yoneda[F, A]): cats.free.Yoneda[F0, A] =
        cats.free.Yoneda[F0, A](inner.apply(fa.run))
    }

  implicit def scalazToCatsYoneda[F[_], F0[_], A](inner: scalaz.Yoneda[F, A])(implicit T: NaturalTransformation[F, F0], F: cats.Functor[F0]): cats.free.Yoneda[F0, A] =
    scalazToCatsYonedaNaturalTransformation[F, F0].apply[A](inner)
}

object YonedaConverter extends YonedaConverter
