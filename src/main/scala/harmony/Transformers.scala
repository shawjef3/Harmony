package harmony

trait Transformers {

  implicit def coproduct[F[_], G[_], F0[_], G0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[G, G0], n2: NaturalTransformation[F0, F], n3: NaturalTransformation[G0, G]): ReversableNatTrans[cats.data.Coproduct[F, G, ?], scalaz.Coproduct[F0, G0, ?]] = {
    import harmony.tocats.data.CoproductConverter._
    import harmony.toscalaz.data.CoproductConverter._

    ReversableNatTrans.fromTransformations[cats.data.Coproduct[F, G, ?], scalaz.Coproduct[F0, G0, ?]](catsToScalazCoproductNaturalTransformation[F, G, F0, G0], scalazToCatsCoproductNaturalTransformation[F0, G0, F, G])
  }

  implicit def coyoneda[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableNatTrans[cats.free.Coyoneda[F, ?], scalaz.Coyoneda[F0, ?]] = {
    import harmony.tocats.data.CoyonedaConverter._
    import harmony.toscalaz.data.CoyonedaConverter._

//    ReversableNatTrans.fromTransformations[cats.free.Coyoneda[F, ?], scalaz.Coyoneda[F0, ?]](catsToScalazCoyonedaNaturalTransformation[F, F0], scalazToCatsCoyonedaNaturalTransformation[F0, F])
  implicitly[ReversableNatTrans[cats.free.Coyoneda[F, ?], scalaz.Coyoneda[F0, ?]]]
  }

  implicit val disjunction: ReversableBiNatTrans[scala.Either[?, ?], scalaz.Disjunction[?, ?]] = {
    import harmony.tocats.data.DisjunctionConverter._
    import harmony.toscalaz.data.EitherConverter._

    ReversableBiNatTrans.fromTransformations[scala.Either[?, ?], scalaz.Disjunction[?, ?]]
  }

  implicit def eitherT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], F: cats.Functor[F0], n1: NaturalTransformation[F0, F], F0: scalaz.Functor[F0]): ReversableBiNatTrans[cats.data.EitherT[F, ?, ?], scalaz.EitherT[F0, ?, ?]] = {
    import harmony.tocats.data.EitherTConverter._
    import harmony.toscalaz.data.EitherTConverter._

//    ReversableBiNatTrans.fromTransformations[cats.data.EitherT[F, ?, ?], scalaz.EitherT[F0, ?, ?]](catsToScalazEitherTBiNaturalTransformation[F, F0](n0, F0), scalazToCatsEitherTBiNaturalTransformation[F0, F](n1, F))
    implicitly[ReversableBiNatTrans[cats.data.EitherT[F, ?, ?], scalaz.EitherT[F0, ?, ?]]]
  }

  implicit def idT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableNatTrans[cats.data.IdT[F, ?], scalaz.IdT[F0, ?]] = {
    import harmony.tocats.data.IdTConverter._
    import harmony.toscalaz.data.IdTConverter._

    implicitly[ReversableNatTrans[cats.data.IdT[F, ?], scalaz.IdT[F0, ?]]]
  }

  implicit def kleisli[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableBiNatTrans[cats.data.Kleisli[F, ?, ?], scalaz.Kleisli[F0, ?, ?]] = {
    import harmony.tocats.data.KleisliConverter._
    import harmony.toscalaz.data.KleisliConverter._

//    ReversableBiNatTrans.fromTransformations[cats.data.Kleisli[F, ?, ?], scalaz.Kleisli[F0, ?, ?]](catsToScalazKleisliBiNaturalTransformation[F, F0], scalazToCatsKleisliBiNaturalTransformation[F0, F])
    implicitly[ReversableBiNatTrans[cats.data.Kleisli[F, ?, ?], scalaz.Kleisli[F0, ?, ?]]]
  }

  implicit def maybeT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F], catsF0: cats.Functor[F0], scalazF0: scalaz.Functor[F0]): ReversableNatTrans[cats.data.OptionT[F, ?], scalaz.MaybeT[F0, ?]] = {
    import harmony.tocats.data.MaybeTConverter._
    import harmony.toscalaz.data.OptionTToMaybeTConverter._

    ReversableNatTrans.fromTransformations[cats.data.OptionT[F, ?], scalaz.MaybeT[F0, ?]](catsToScalazMaybeTNaturalTransformation[F, F0], scalazMaybeTToCatsOptionTNaturalTransformation[F0, F])
  }

  implicit val name: ReversableNatTrans[cats.Eval, scalaz.Name] = {
    import harmony.tocats.data.NameConverter._
    import harmony.toscalaz.data.EvalConverter._

    ReversableNatTrans.fromTransformations[cats.Eval, scalaz.Name]
  }

  implicit val nonEmptyList: ReversableNatTrans[cats.data.NonEmptyList, scalaz.NonEmptyList] = {
    import harmony.tocats.data.NonEmptyListConverter._
    import harmony.toscalaz.data.NonEmptyListConverter._

    ReversableNatTrans.fromTransformations[cats.data.NonEmptyList, scalaz.NonEmptyList]
  }

  implicit val nonEmptyVector: ReversableNatTrans[cats.data.NonEmptyVector, scalaz.OneAnd[Vector, ?]] = {
    import harmony.tocats.data.OneAndConverter._
    import harmony.toscalaz.data.NonEmptyVectorConverter._

    ReversableNatTrans.fromTransformations[cats.data.NonEmptyVector, scalaz.OneAnd[Vector, ?]]
  }

  implicit def oneAnd[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableNatTrans[cats.data.OneAnd[F, ?], scalaz.OneAnd[F0, ?]] = {
    import harmony.tocats.data.OneAndConverter._
    import harmony.toscalaz.data.OneAndConverter._

    ReversableNatTrans.fromTransformations[cats.data.OneAnd[F, ?], scalaz.OneAnd[F0, ?]](catsToScalazOneAndListNaturalTransformation[F, F0], scalazToCatsOneAndNaturalTransformation[F0, F])
  }

  implicit val oneAndStream: ReversableNatTrans[cats.data.NonEmptyStream, scalaz.OneAnd[Stream, ?]] = {
    implicit val n = NaturalTransformation.refl[Stream]
    oneAnd[Stream, Stream]
  }

  implicit val option: ReversableNatTrans[scala.Option, scalaz.Maybe] = {
    import harmony.tocats.data.MaybeConverter._
    import harmony.toscalaz.data.OptionToMaybeConverter._

    ReversableNatTrans.fromTransformations[scala.Option[?], scalaz.Maybe[?]]
  }

  implicit def optionT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableNatTrans[cats.data.OptionT[F, ?], scalaz.OptionT[F0, ?]] = {
    import harmony.tocats.data.OptionTConverter._
    import harmony.toscalaz.data.OptionTToOptionTConverter._

    ReversableNatTrans.fromTransformations[cats.data.OptionT[F, ?], scalaz.OptionT[F0, ?]](catsToScalazOptionTNaturalTransformation[F, F0], scalazOptionTToCatsOptionTNaturalTransformation[F0, F])
  }

  implicit def stateT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], F: cats.FlatMap[F], A: cats.Applicative[F], n1: NaturalTransformation[F0, F], F0: scalaz.Monad[F0]): ReversableBiNatTrans[cats.data.StateT[F, ?, ?], scalaz.StateT[F0, ?, ?]] = {
    import harmony.tocats.data.StateTConverter._
    import harmony.toscalaz.data.StateTConverter._

    ReversableBiNatTrans.fromTransformations[cats.data.StateT[F, ?, ?], scalaz.StateT[F0, ?, ?]](catsToScalazStateTBiNaturalTransformation[F, F0], scalazToCatsStateTBiNaturalTransformation[F0, F])
  }

  implicit val validationNel: ReversableBiNatTrans[cats.data.ValidatedNel[?, ?], scalaz.ValidationNel[?, ?]] = {
    import harmony.tocats.data.ValidationNelConverter._
    import harmony.toscalaz.data.ValidatedNelConverter._

    ReversableBiNatTrans.fromTransformations[cats.data.ValidatedNel[?, ?], scalaz.ValidationNel[?, ?]]
  }

  implicit def writerT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableBiNatTrans[cats.data.WriterT[F, ?, ?], scalaz.WriterT[F0, ?, ?]] = {
    import harmony.tocats.data.WriterTConverter._
    import harmony.toscalaz.data.WriterTConverter._

    ReversableBiNatTrans.fromTransformations[cats.data.WriterT[F, ?, ?], scalaz.WriterT[F0, ?, ?]](catsToScalazWriterTBiNaturalTransformation[F, F0], scalazToCatsWriterTBiNaturalTransformation[F0, F])
  }

  implicit def yoneda[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], F: scalaz.Functor[F0], n1: NaturalTransformation[F0, F], F0: cats.Functor[F]): ReversableNatTrans[cats.free.Yoneda[F, ?], scalaz.Yoneda[F0, ?]] = {
    import harmony.tocats.data.YonedaConverter._
    import harmony.toscalaz.data.YonedaConverter._

    ReversableNatTrans.fromTransformations[cats.free.Yoneda[F, ?], scalaz.Yoneda[F0, ?]](catsToScalazYonedaNaturalTransformation[F, F0], scalazToCatsYonedaNaturalTransformation[F0, F])
  }

}

object Transformers extends Transformers
