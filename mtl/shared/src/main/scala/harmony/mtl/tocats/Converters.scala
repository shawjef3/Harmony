package harmony.mtl.tocats
import harmony.tocats.typeclass.FunctorConverter

trait CatsApplicativeLocal[F[_], S] {
  self: cats.mtl.ApplicativeLocal[F, S] =>

  protected implicit def scalazMonadReader: scalaz.MonadReader[F, S]

  override def local[A](f: (S) => S)(fa: F[A]): F[A] =
    scalazMonadReader.local(f)(fa)

  override def scope[A](e: S)(fa: F[A]): F[A] =
    scalazMonadReader.scope(e)(fa)

}

trait ApplicativeLocalConverter {

  implicit def scalazToCatsApplicativeLocal[F[_], S](implicit inner: scalaz.MonadReader[F, S]): cats.mtl.ApplicativeLocal[F, S] =
    new CatsApplicativeLocal[F, S] with cats.mtl.ApplicativeLocal[F, S] {
      override protected implicit def scalazMonadReader: scalaz.MonadReader[F, S] = inner

      override lazy val ask: cats.mtl.ApplicativeAsk[F, S] =
        new cats.mtl.ApplicativeAsk[F, S] {
          override val applicative: cats.Applicative[F] =
            harmony.tocats.typeclass.ApplicativeConverter.scalazToCatsApplicativeValue[F](scalazMonadReader)

          override def ask: F[S] = inner.ask

          override def reader[A](f: (S) => A): F[A] = inner.asks(f)
        }
    }

  implicit def scalazToCatsApplicativeLocalValue[F[_], S](inner: scalaz.MonadReader[F, S]): cats.mtl.ApplicativeLocal[F, S] =
    scalazToCatsApplicativeLocal[F, S](inner)

}

object ApplicativeLocalConverter extends ApplicativeLocalConverter

trait CatsFunctorListen[F[_], S] {
  self: cats.mtl.FunctorListen[F, S] =>

  protected implicit def scalazMonadListen: scalaz.MonadListen[F, S]

  override lazy val tell: cats.mtl.FunctorTell[F, S] =
    new cats.mtl.FunctorTell[F, S] {
      override val functor: cats.Functor[F] =
        FunctorConverter.scalazToCatsFunctorValue(scalazMonadListen)

      override def tell(l: S): F[Unit] = scalazMonadListen.tell(l)

      override def writer[A](a: A, l: S): F[A] = scalazMonadListen.writer(l, a)

      override def tuple[A](ta: (S, A)): F[A] =
        functor.map(tell(ta._1))(Function.const(ta._2))
    }

  override def listen[A](fa: F[A]): F[(A, S)] = scalazMonadListen.listen(fa)

  override def listens[A, B](fa: F[A])(f: S => B): F[(A, B)] =
    tell.functor.map(scalazMonadListen.listen(fa)) {
      case (n, o) =>
        (n, f(o))
    }
}

trait FunctorListenConverter {

  implicit def scalazToCatsFunctorListen[F[_], S](implicit inner: scalaz.MonadListen[F, S]): cats.mtl.FunctorListen[F, S] =
    new CatsFunctorListen[F, S] with cats.mtl.FunctorListen[F, S] {
      override protected implicit def scalazMonadListen: scalaz.MonadListen[F, S] = inner
    }

  implicit def scalazToCatsFunctorListenValue[F[_], S](inner: scalaz.MonadListen[F, S]): cats.mtl.FunctorListen[F, S] =
    scalazToCatsFunctorListen[F, S](inner)

}

object FunctorListenConverter extends FunctorListenConverter

trait CatsMonadState[F[_], S] {
  self: cats.mtl.MonadState[F, S] =>

  protected implicit def scalazMonadState: scalaz.MonadState[F, S]

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

  implicit def scalazToCatsMonadState[F[_], S](implicit inner: scalaz.MonadState[F, S], inner0: cats.Monad[F]): cats.mtl.MonadState[F, S] =
    new CatsMonadState[F, S] with cats.mtl.MonadState[F, S] {
      override protected implicit def scalazMonadState: scalaz.MonadState[F, S] = inner
      override val monad: cats.Monad[F] = inner0
    }

  implicit def scalazToCatsMonadStateValue[F[_], S](inner: scalaz.MonadState[F, S])(implicit inner0: cats.Monad[F]): cats.mtl.MonadState[F, S] =
    scalazToCatsMonadState[F, S](inner, inner0)
}

object MonadStateConverter extends MonadStateConverter
