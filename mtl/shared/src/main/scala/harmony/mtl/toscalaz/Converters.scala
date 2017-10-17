package harmony.mtl.toscalaz

import harmony.toscalaz.typeclass.{ScalazApplicative, ScalazBindRec, ScalazInvariantFunctor}

/*
Cats ApplicativeLocal isn't a Monad, and so it's not directly convertible to scalaz.MonadReader. It also needs a FlatMap.
 */

trait ScalazMonadReader[F[_], S] {
  self: scalaz.MonadReader[F, S] =>

  protected implicit def catsApplicativeLocal: cats.mtl.ApplicativeLocal[F, S]

  override def ask: F[S] = catsApplicativeLocal.ask.ask

  override def local[A](f: S => S)(fa: F[A]): F[A] = catsApplicativeLocal.local(f)(fa)
}

trait MonadReaderConverter {

  implicit def catsToScalazMonadReaderValue[F[_], S](inner: cats.mtl.ApplicativeLocal[F, S])(implicit innerMonad: scalaz.Monad[F]): scalaz.MonadReader[F, S] =
    new ScalazMonadReader[F, S] with scalaz.MonadReader[F, S] {
      override protected implicit def catsApplicativeLocal: cats.mtl.ApplicativeLocal[F, S] = inner

      override def point[A](a: => A): F[A] = innerMonad.point(a)

      override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = innerMonad.bind(fa)(f)
    }

  implicit def catsToScalazMonadReader[F[_], S](implicit inner: cats.mtl.ApplicativeLocal[F, S], innerMonad: scalaz.Monad[F]): scalaz.MonadReader[F, S] =
    catsToScalazMonadReaderValue[F, S](inner)(innerMonad)

}

object MonadReaderConverter
  extends MonadReaderConverter

/*
Cats FunctorListen isn't a Monad, and so it's not directly convertible to scalaz.MonadReader. It also needs a BindRec.
 */

trait ScalazMonadTell[F[_], S] {
  self: scalaz.MonadTell[F, S] =>

  protected implicit def catsFunctorTell: cats.mtl.FunctorTell[F, S]

  override def writer[A](w: S, v: A): F[A] = catsFunctorTell.writer(v, w)
}

trait MonadTellConverter {
  implicit def catsToScalazMonadTellValue[F[_], S](inner: cats.mtl.FunctorTell[F, S])(implicit innerMonad: scalaz.Monad[F]): scalaz.MonadTell[F, S] =
    new ScalazMonadTell[F, S] with scalaz.MonadTell[F, S] {
      override protected implicit def catsFunctorTell: cats.mtl.FunctorTell[F, S] = inner

      override def point[A](a: => A): F[A] = innerMonad.point(a)

      override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = innerMonad.bind(fa)(f)
    }

  implicit def catsToScalazMonadTell[F[_], S](implicit inner: cats.mtl.FunctorTell[F, S], innerMonad: scalaz.Monad[F]): scalaz.MonadTell[F, S] =
    catsToScalazMonadTellValue[F, S](inner)(innerMonad)
}

object MonadTellConverter
  extends MonadTellConverter

trait ScalazMonadState[F[_], S]
  extends ScalazApplicative[F]
    with ScalazBindRec[F] {
  self: scalaz.MonadState[F, S] with scalaz.BindRec[F] =>

  protected implicit def catsMonadState: cats.mtl.MonadState[F, S]
  override protected implicit def catsApplicative: cats.Applicative[F] = catsMonadState.monad
  override protected implicit def catsFlatMap: cats.FlatMap[F] = catsMonadState.monad

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
      override protected implicit def catsMonadState: cats.mtl.MonadState[F, S] = inner

      override def xmap[A, B](ma: F[A], f: (A) => B, g: (B) => A): F[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant)
    }

  implicit def catsToScalazMonadStateValue[F[_], S](inner: cats.mtl.MonadState[F, S]): scalaz.MonadState[F, S] =
    catsToScalazMonadState[F, S](inner)

}

object MonadStateConverter extends MonadStateConverter
