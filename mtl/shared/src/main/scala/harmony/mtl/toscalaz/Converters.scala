package harmony.mtl.toscalaz

import harmony.toscalaz.typeclass.{ScalazApplicative, ScalazBindRec, ScalazInvariantFunctor}

/*
Cats ApplicativeLocal isn't a Monad, and so it's not convertible to scalaz.MonadReader.
 */

/*
Cats FunctorListen isn't a Monad, and so it's not convertible to scalaz.MonadReader.
 */

trait ScalazMonadState[F[_], S]
  extends ScalazApplicative[F]
    with ScalazBindRec[F] {
  self: scalaz.MonadState[F, S] with scalaz.BindRec[F] =>

  protected implicit def catsMonadState: cats.mtl.MonadState[F, S]
  override protected implicit lazy val catsApplicative: cats.Applicative[F] = catsMonadState.monad
  override protected implicit lazy val catsFlatMap: cats.FlatMap[F] = catsMonadState.monad

  override def init: F[S] =
    get

  override def get: F[S] =
    catsMonadState.get

  override def put(s: S): F[Unit] =
    catsMonadState.set(s)
}

trait MonadStateConverter {

  implicit def catsToScalazMonadState[F[_], S](implicit inner: cats.mtl.MonadState[F, S]): scalaz.MonadState[F, S] =
    new ScalazMonadState[F, S] with scalaz.MonadState[F, S] with scalaz.BindRec[F] {

      override protected implicit val catsMonadState: cats.mtl.MonadState[F, S] = inner

      override def xmap[A, B](ma: F[A], f: (A) => B, g: (B) => A): F[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant)
    }

  implicit def catsToScalazMonadStateValue[F[_], S](inner: cats.mtl.MonadState[F, S]): scalaz.MonadState[F, S] =
    catsToScalazMonadState[F, S](inner)

}

object MonadStateConverter extends MonadStateConverter
