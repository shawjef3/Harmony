package harmony

trait Transformers {

  implicit def coproduct[F[_], G[_], F0[_], G0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[G, G0], n2: NaturalTransformation[F0, F], n3: NaturalTransformation[G0, G]): ReversableNatTrans[cats.data.Coproduct[F, G, ?], scalaz.Coproduct[F0, G0, ?], scalaz.Coproduct[F0, G0, ?], cats.data.Coproduct[F, G, ?]] = {
    import harmony.tocats.data.CoproductConverter._
    import harmony.toscalaz.data.CoproductConverter._

    ReversableNatTrans.fromTransformations[cats.data.Coproduct[F, G, ?], scalaz.Coproduct[F0, G0, ?], scalaz.Coproduct[F0, G0, ?], cats.data.Coproduct[F, G, ?]](catsToScalazCoproductNaturalTransformation[F, G, F0, G0], scalazToCatsCoproductNaturalTransformation[F0, G0, F, G])
  }

  implicit def coyoneda[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableNatTrans[cats.free.Coyoneda[F, ?], scalaz.Coyoneda[F0, ?], scalaz.Coyoneda[F0, ?], cats.free.Coyoneda[F, ?]] = {
    import harmony.tocats.data.CoyonedaConverter._
    import harmony.toscalaz.data.CoyonedaConverter._

//    ReversableNatTrans.fromTransformations[cats.free.Coyoneda[F, ?], scalaz.Coyoneda[F0, ?]](catsToScalazCoyonedaNaturalTransformation[F, F0], scalazToCatsCoyonedaNaturalTransformation[F0, F])
  implicitly[ReversableNatTrans[cats.free.Coyoneda[F, ?], scalaz.Coyoneda[F0, ?], scalaz.Coyoneda[F0, ?], cats.free.Coyoneda[F, ?]]]
  }

  implicit val disjunction: ReversableBiNatTrans[scala.Either[?, ?], scalaz.Disjunction[?, ?], scalaz.Disjunction[?, ?], scala.Either[?, ?]] = {
    import harmony.tocats.data.DisjunctionConverter._
    import harmony.toscalaz.data.EitherConverter._

    ReversableBiNatTrans.fromTransformations[scala.Either[?, ?], scalaz.Disjunction[?, ?], scalaz.Disjunction[?, ?], scala.Either[?, ?]]
  }

  implicit def eitherT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], F: cats.Functor[F0], n1: NaturalTransformation[F0, F], F0: scalaz.Functor[F0]): ReversableBiNatTrans[cats.data.EitherT[F, ?, ?], scalaz.EitherT[F0, ?, ?], scalaz.EitherT[F0, ?, ?], cats.data.EitherT[F, ?, ?]] = {
    import harmony.tocats.data.EitherTConverter._
    import harmony.toscalaz.data.EitherTConverter._

//    ReversableBiNatTrans.fromTransformations[cats.data.EitherT[F, ?, ?], scalaz.EitherT[F0, ?, ?]](catsToScalazEitherTBiNaturalTransformation[F, F0](n0, F0), scalazToCatsEitherTBiNaturalTransformation[F0, F](n1, F))
    implicitly[ReversableBiNatTrans[cats.data.EitherT[F, ?, ?], scalaz.EitherT[F0, ?, ?], scalaz.EitherT[F0, ?, ?], cats.data.EitherT[F, ?, ?]]]
  }

  implicit def idT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableNatTrans[cats.data.IdT[F, ?], scalaz.IdT[F0, ?], scalaz.IdT[F0, ?], cats.data.IdT[F, ?]] = {
    import harmony.tocats.data.IdTConverter._
    import harmony.toscalaz.data.IdTConverter._

    implicitly[ReversableNatTrans[cats.data.IdT[F, ?], scalaz.IdT[F0, ?], scalaz.IdT[F0, ?], cats.data.IdT[F, ?]]]
  }

  implicit def kleisli[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableBiNatTrans[cats.data.Kleisli[F, ?, ?], scalaz.Kleisli[F0, ?, ?], scalaz.Kleisli[F0, ?, ?], cats.data.Kleisli[F, ?, ?]] = {
    import harmony.tocats.data.KleisliConverter._
    import harmony.toscalaz.data.KleisliConverter._

//    ReversableBiNatTrans.fromTransformations[cats.data.Kleisli[F, ?, ?], scalaz.Kleisli[F0, ?, ?]](catsToScalazKleisliBiNaturalTransformation[F, F0], scalazToCatsKleisliBiNaturalTransformation[F0, F])
    implicitly[ReversableBiNatTrans[cats.data.Kleisli[F, ?, ?], scalaz.Kleisli[F0, ?, ?], scalaz.Kleisli[F0, ?, ?], cats.data.Kleisli[F, ?, ?]]]
  }

  implicit def maybeT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F], catsF0: cats.Functor[F0], scalazF0: scalaz.Functor[F0]): ReversableNatTrans[cats.data.OptionT[F, ?], scalaz.MaybeT[F0, ?], scalaz.MaybeT[F0, ?], cats.data.OptionT[F, ?]] = {
    import harmony.tocats.data.MaybeTConverter._
    import harmony.toscalaz.data.OptionTToMaybeTConverter._

    ReversableNatTrans.fromTransformations[cats.data.OptionT[F, ?], scalaz.MaybeT[F0, ?], scalaz.MaybeT[F0, ?], cats.data.OptionT[F, ?]](catsToScalazMaybeTNaturalTransformation[F, F0], scalazMaybeTToCatsOptionTNaturalTransformation[F0, F])
  }

  implicit val name: ReversableNatTrans[cats.Eval, scalaz.Name, scalaz.Name, cats.Eval] = {
    import harmony.tocats.data.NameConverter._
    import harmony.toscalaz.data.EvalConverter._

    ReversableNatTrans.fromTransformations[cats.Eval, scalaz.Name, scalaz.Name, cats.Eval]
  }

  implicit val nonEmptyList: ReversableNatTrans[cats.data.NonEmptyList, scalaz.NonEmptyList, scalaz.NonEmptyList, cats.data.NonEmptyList] = {
    import harmony.tocats.data.NonEmptyListConverter._
    import harmony.toscalaz.data.NonEmptyListConverter._

    ReversableNatTrans.fromTransformations[cats.data.NonEmptyList, scalaz.NonEmptyList, scalaz.NonEmptyList, cats.data.NonEmptyList]
  }

  implicit val nonEmptyVector: ReversableNatTrans[cats.data.NonEmptyVector, scalaz.OneAnd[Vector, ?], scalaz.OneAnd[Vector, ?], cats.data.NonEmptyVector] = {
    import harmony.tocats.data.OneAndConverter._
    import harmony.toscalaz.data.NonEmptyVectorConverter._

    ReversableNatTrans.fromTransformations[cats.data.NonEmptyVector, scalaz.OneAnd[Vector, ?], scalaz.OneAnd[Vector, ?], cats.data.NonEmptyVector]
  }

  implicit def oneAnd[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableNatTrans[cats.data.OneAnd[F, ?], scalaz.OneAnd[F0, ?], scalaz.OneAnd[F0, ?], cats.data.OneAnd[F, ?]] = {
    import harmony.tocats.data.OneAndConverter._
    import harmony.toscalaz.data.OneAndConverter._

    ReversableNatTrans.fromTransformations[cats.data.OneAnd[F, ?], scalaz.OneAnd[F0, ?], scalaz.OneAnd[F0, ?], cats.data.OneAnd[F, ?]](catsToScalazOneAndListNaturalTransformation[F, F0], scalazToCatsOneAndNaturalTransformation[F0, F])
  }

  implicit val oneAndStream: ReversableNatTrans[cats.data.NonEmptyStream, scalaz.OneAnd[Stream, ?], scalaz.OneAnd[Stream, ?], cats.data.NonEmptyStream] = {
    implicit val n = NaturalTransformation.refl[Stream]
    oneAnd[Stream, Stream]
  }

  implicit val option: ReversableNatTrans[scala.Option, scalaz.Maybe, scalaz.Maybe, scala.Option] = {
    import harmony.tocats.data.MaybeConverter._
    import harmony.toscalaz.data.OptionToMaybeConverter._

    ReversableNatTrans.fromTransformations[scala.Option[?], scalaz.Maybe[?], scalaz.Maybe[?], scala.Option[?]]
  }

  implicit def optionT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableNatTrans[cats.data.OptionT[F, ?], scalaz.OptionT[F0, ?], scalaz.OptionT[F0, ?], cats.data.OptionT[F, ?]] = {
    import harmony.tocats.data.OptionTConverter._
    import harmony.toscalaz.data.OptionTToOptionTConverter._

    ReversableNatTrans.fromTransformations[cats.data.OptionT[F, ?], scalaz.OptionT[F0, ?], scalaz.OptionT[F0, ?], cats.data.OptionT[F, ?]](catsToScalazOptionTNaturalTransformation[F, F0], scalazOptionTToCatsOptionTNaturalTransformation[F0, F])
  }

  implicit def stateT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], F: cats.FlatMap[F], A: cats.Applicative[F], n1: NaturalTransformation[F0, F], F0: scalaz.Monad[F0]): ReversableBiNatTrans[cats.data.StateT[F, ?, ?], scalaz.StateT[F0, ?, ?], scalaz.StateT[F0, ?, ?], cats.data.StateT[F, ?, ?]] = {
    import harmony.tocats.data.StateTConverter._
    import harmony.toscalaz.data.StateTConverter._

    ReversableBiNatTrans.fromTransformations[cats.data.StateT[F, ?, ?], scalaz.StateT[F0, ?, ?], scalaz.StateT[F0, ?, ?], cats.data.StateT[F, ?, ?]](catsToScalazStateTBiNaturalTransformation[F, F0], scalazToCatsStateTBiNaturalTransformation[F0, F])
  }

  implicit val validationNel: ReversableBiNatTrans[cats.data.ValidatedNel[?, ?], scalaz.ValidationNel[?, ?], scalaz.ValidationNel[?, ?], cats.data.ValidatedNel[?, ?]] = {
    import harmony.tocats.data.ValidationNelConverter._
    import harmony.toscalaz.data.ValidatedNelConverter._

    ReversableBiNatTrans.fromTransformations[cats.data.ValidatedNel[?, ?], scalaz.ValidationNel[?, ?], scalaz.ValidationNel[?, ?], cats.data.ValidatedNel[?, ?]]
  }

  implicit def writerT[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], n1: NaturalTransformation[F0, F]): ReversableBiNatTrans[cats.data.WriterT[F, ?, ?], scalaz.WriterT[F0, ?, ?], scalaz.WriterT[F0, ?, ?], cats.data.WriterT[F, ?, ?]] = {
    import harmony.tocats.data.WriterTConverter._
    import harmony.toscalaz.data.WriterTConverter._

    ReversableBiNatTrans.fromTransformations[cats.data.WriterT[F, ?, ?], scalaz.WriterT[F0, ?, ?], scalaz.WriterT[F0, ?, ?], cats.data.WriterT[F, ?, ?]](catsToScalazWriterTBiNaturalTransformation[F, F0], scalazToCatsWriterTBiNaturalTransformation[F0, F])
  }

  implicit def yoneda[F[_], F0[_]](implicit n0: NaturalTransformation[F, F0], F: scalaz.Functor[F0], n1: NaturalTransformation[F0, F], F0: cats.Functor[F]): ReversableNatTrans[cats.free.Yoneda[F, ?], scalaz.Yoneda[F0, ?], scalaz.Yoneda[F0, ?], cats.free.Yoneda[F, ?]] = {
    import harmony.tocats.data.YonedaConverter._
    import harmony.toscalaz.data.YonedaConverter._

    ReversableNatTrans.fromTransformations[cats.free.Yoneda[F, ?], scalaz.Yoneda[F0, ?], scalaz.Yoneda[F0, ?], cats.free.Yoneda[F, ?]](catsToScalazYonedaNaturalTransformation[F, F0], scalazToCatsYonedaNaturalTransformation[F0, F])
  }

}

object Transformers extends Transformers
