package harmony.mtl.tocats
import scalaz.BindRec

trait CatsApplicativeLocal[F[_], S] {
  self: cats.mtl.ApplicativeLocal[F, S] =>

  protected implicit def scalazMonadReader: scalaz.MonadReader[F, S]

  override def local[A](f: (S) => S)(fa: F[A]): F[A] =
    scalazMonadReader.local(f)(fa)

  override def scope[A](e: S)(fa: F[A]): F[A] =
    scalazMonadReader.scope(e)(fa)

}

trait ApplicativeLocalConverter {

  implicit def scalazToCatsApplicativeLocalInstance[F[_], S](implicit inner: scalaz.MonadReader[F, S]): cats.mtl.ApplicativeLocal[F, S] =
    new CatsApplicativeLocal[F, S] with cats.mtl.ApplicativeLocal[F, S] {
      override protected implicit def scalazMonadReader: scalaz.MonadReader[F, S] = inner

      override val ask: cats.mtl.ApplicativeAsk[F, S] =
        new cats.mtl.ApplicativeAsk[F, S] {
          override val applicative: cats.Applicative[F] =
            harmony.tocats.typeclass.ApplicativeConverter.scalazToCatsApplicativeValue[F](scalazMonadReader)

          override def ask: F[S] = inner.ask

          override def reader[A](f: (S) => A): F[A] = inner.asks(f)
        }
    }

  implicit def scalazToCatsApplicativeLocalValue[F[_], S](inner: scalaz.MonadReader[F, S]): cats.mtl.ApplicativeLocal[F, S] =
    scalazToCatsApplicativeLocalInstance[F, S](inner)

}

object ApplicativeLocalConverter extends ApplicativeLocalConverter

trait CatsMonadState[F[_], S] {
  self: cats.mtl.MonadState[F, S] =>

  protected implicit def scalazMonadState: scalaz.MonadState[F, S]

  protected implicit def scalazBindRec: scalaz.BindRec[F]

  override lazy val monad: cats.Monad[F] =
    harmony.tocats.typeclass.MonadConverter.scalazToCatsMonadValue[F](scalazMonadState)

  override def get: F[S] =
    scalazMonadState.get

  override def set(s: S): F[Unit] =
    scalazMonadState.put(s)

  override def inspect[A](f: (S) => A): F[A] =
    scalazMonadState.gets(f)

  override def modify(f: (S) => S): F[Unit] =
    scalazMonadState.modify(f)
}

trait MonadStateConverter {

  implicit def scalazToCatsMonadStateInstance[F[_], S](implicit inner: scalaz.MonadState[F, S], inner0: scalaz.BindRec[F]): cats.mtl.MonadState[F, S] =
    new CatsMonadState[F, S] with cats.mtl.MonadState[F, S] {
      override protected implicit val scalazMonadState: scalaz.MonadState[F, S] = inner
      override protected implicit val scalazBindRec: BindRec[F] = inner0
    }

  implicit def scalazToCatsMonadStateValue[F[_], S](inner: scalaz.MonadState[F, S])(implicit inner0: scalaz.BindRec[F]): cats.mtl.MonadState[F, S] =
    scalazToCatsMonadStateInstance[F, S](inner, inner0)
}

object MonadStateConverter extends MonadStateConverter
