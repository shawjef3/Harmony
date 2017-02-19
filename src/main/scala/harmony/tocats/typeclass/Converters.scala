package harmony.tocats.typeclass

trait CatsAlternative[F[_]] extends CatsApplicative[F] {
  self: cats.Alternative[F] =>

  protected implicit val scalazApplicativePlus: scalaz.ApplicativePlus[F]
  override protected implicit val scalazApplicative: scalaz.Applicative[F] = scalazApplicativePlus

  override def empty[A]: F[A] =
    scalazApplicativePlus.empty[A]

  override def combineK[A](x: F[A], y: F[A]): F[A] =
    scalazApplicativePlus.plus(x, y)

}

trait AlternativeConverter {

  implicit def scalazToCatsAlternativeInstance[F[_]](implicit inner: scalaz.ApplicativePlus[F]): cats.Alternative[F] =
    new CatsAlternative[F] with cats.Alternative[F] {
      override protected implicit val scalazApplicativePlus: scalaz.ApplicativePlus[F] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsAlternativeValue[F[_]](inner: scalaz.ApplicativePlus[F]): cats.Alternative[F] =
    scalazToCatsAlternativeInstance[F](inner)

}

object AlternativeConverter extends AlternativeConverter

trait CatsApplicative[F[_]] extends CatsApply[F] {
  self: cats.Applicative[F] =>

  protected implicit val scalazApplicative: scalaz.Applicative[F]
  override protected implicit val scalazApply: scalaz.Apply[F] = scalazApplicative

  override def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] =
    scalazApplicative.ap(fa)(ff)

  override def pure[A](x: A): F[A] =
    scalazApplicative.point(x)

}

trait ApplicativeConverter {

  implicit def scalazToCatsApplicativeInstance[F[_]](implicit inner: scalaz.Applicative[F]): cats.Applicative[F] =
    new CatsApplicative[F] with cats.Applicative[F] {
      override protected implicit val scalazApplicative: scalaz.Applicative[F] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsApplicativeValue[F[_]](inner: scalaz.Applicative[F]): cats.Applicative[F] =
    scalazToCatsApplicativeInstance[F](inner)

}

object ApplicativeConverter extends ApplicativeConverter

trait CatsArrow[F[_, _]] {
  self: cats.arrow.Arrow[F] =>

  protected implicit val scalazArrow: scalaz.Arrow[F]

  override def lift[A, B](f: (A) => B): F[A, B] =
    scalazArrow.arr(f)

  override def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] =
    scalazArrow.first(fa)

  override def id[A]: F[A, A] =
    scalazArrow.id[A]

  override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
    scalazArrow.compose(f, g)
}

trait ArrowConverter {

  implicit def scalazToCatsArrowInstance[F[_, _]](implicit inner: scalaz.Arrow[F]): cats.arrow.Arrow[F] =
    new CatsArrow[F] with cats.arrow.Arrow[F] {
      override protected implicit val scalazArrow: scalaz.Arrow[F] = inner
    }

  implicit def scalazToCatsArrowValue[F[_, _]](inner: scalaz.Arrow[F]): cats.arrow.Arrow[F] =
    scalazToCatsArrowInstance[F](inner)

}

object ArrowConverter extends ArrowConverter

trait CatsApply[F[_]] extends CatsFunctor[F] {
  self: cats.Apply[F] =>

  protected implicit val scalazApply: scalaz.Apply[F]
  override protected implicit val scalazFunctor: scalaz.Functor[F] = scalazApply

  override def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] =
    scalazApply.ap(fa)(ff)

}

trait ApplyConverter {

  implicit def scalazToCatsApplyInstance[F[_]](implicit inner: scalaz.Apply[F]): cats.Apply[F] =
    new CatsApply[F] with cats.Apply[F] {
      override protected implicit val scalazApply: scalaz.Apply[F] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsApplyValue[F[_]](inner: scalaz.Apply[F]): cats.Apply[F] =
    scalazToCatsApplyInstance[F](inner)

}

object ApplyConverter extends ApplyConverter

trait CatsBifoldable[F[_, _]] {
  self: cats.Bifoldable[F] =>

  import cats.Eval

  protected implicit val scalazBifoldable: scalaz.Bifoldable[F]

  override def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    scalazBifoldable.bifoldLeft(fab, c)(f)(g)

  override def bifoldRight[A, B, C](
    fab: F[A, B],
    c: Eval[C]
  )(f: (A, Eval[C]) => Eval[C],
    g: (B, Eval[C]) => Eval[C]
  ): Eval[C] =
    //TODO: is this a correct evaluation laziness?
    Eval.later[C] {
      scalazBifoldable.bifoldRight[A, B, C](
        fab,
        c.value
      )((a, c) => f(a, Eval.now(c)).value
      )((b, c) => g(b, Eval.now(c)).value
      )
    }

}

trait BifoldableConverter {

  implicit def scalazToCatsBifoldableInstance[F[_, _]](implicit inner: scalaz.Bifoldable[F]): cats.Bifoldable[F] =
    new CatsBifoldable[F] with cats.Bifoldable[F] {
      override protected implicit val scalazBifoldable: scalaz.Bifoldable[F] = inner
    }

  implicit def scalazToCatsBifoldableValue[F[_, _]](inner: scalaz.Bifoldable[F]): cats.Bifoldable[F] =
    scalazToCatsBifoldableInstance[F](inner)

}

object BifoldableConverter extends BifoldableConverter

trait CatsBifunctor[F[_, _]] {
  self: cats.functor.Bifunctor[F] =>

  protected implicit val scalazBifunctor: scalaz.Bifunctor[F]

  override def bimap[A, B, C, D](fab: F[A, B])(f: (A) => C, g: (B) => D): F[C, D] =
    scalazBifunctor.bimap(fab)(f, g)

}

trait BifunctorConverter {

  implicit def scalazToCatsBifunctorInstance[F[_, _]](implicit inner: scalaz.Bifunctor[F]): cats.functor.Bifunctor[F] =
    new CatsBifunctor[F] with cats.functor.Bifunctor[F] {
      override protected implicit val scalazBifunctor: scalaz.Bifunctor[F] = inner
    }

  implicit def scalazToCatsBifunctorValue[F[_, _]](inner: scalaz.Bifunctor[F]): cats.functor.Bifunctor[F] =
    scalazToCatsBifunctorInstance[F](inner)

}

object BifunctorConverter extends BifunctorConverter

trait CatsBind[F[_]] extends CatsApply[F] {
  self: cats.FlatMap[F] =>

  protected implicit val scalazBindRec: scalaz.BindRec[F]
  override protected implicit val scalazApply: scalaz.Apply[F] = scalazBindRec

  override def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B] =
    scalazBindRec.bind(fa)(a => f(a))

  override def tailRecM[A, B](a: A)(f: (A) => F[Either[A, B]]): F[B] =
    scalazBindRec.tailrecM((a: A) => scalazBindRec.map(f(a))(scalaz.Disjunction.fromEither))(a)

}

trait BindConverter {

  implicit def scalazToCatsFlatMapInstance[F[_]](implicit inner: scalaz.BindRec[F]): cats.FlatMap[F] =
    new CatsBind[F] with cats.FlatMap[F] {
      override protected implicit val scalazBindRec: scalaz.BindRec[F] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsFlatMapValue[F[_]](inner: scalaz.BindRec[F]): cats.FlatMap[F] =
    scalazToCatsFlatMapInstance[F](inner)

}

object BindConverter extends BindConverter

trait CatsCategory[F[_, _]] {
  self: cats.arrow.Category[F] =>

  protected implicit val scalazCategory: scalaz.Category[F]

  override def id[A]: F[A, A] =
    scalazCategory.id[A]

  override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
    scalazCategory.compose(f, g)

}

trait CategoryConverter {

  implicit def scalazToCatsCategory[F[_, _]](implicit inner: scalaz.Category[F]): cats.arrow.Category[F] =
    new CatsCategory[F] with cats.arrow.Category[F] {
      override protected implicit val scalazCategory: scalaz.Category[F] = inner
    }

  implicit def scalazToCatsCategoryValue[F[_, _]](inner: scalaz.Category[F]): cats.arrow.Category[F] =
    scalazToCatsCategory[F](inner)

}

object CategoryConverter extends CategoryConverter

trait CatsChoice[F[_, _]] {
  self: cats.arrow.Choice[F] =>

  protected implicit val scalazChoice: scalaz.Choice[F]

  override def choice[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C] =
    ???

  override def id[A]: F[A, A] =
    scalazChoice.id[A]

  override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
    scalazChoice.compose(f, g)

}

trait ChoiceConverter {

  implicit def scalazToCatsChoiceInstance[F[_, _]](implicit inner: scalaz.Choice[F]): cats.arrow.Choice[F] =
    new CatsChoice[F] with cats.arrow.Choice[F] {
      override protected implicit val scalazChoice: scalaz.Choice[F] = inner
    }

  implicit def scalazToCatsChoiceValue[F[_, _]](inner: scalaz.Choice[F]): cats.arrow.Choice[F] =
    scalazToCatsChoiceInstance[F](inner)

}

object ChoiceConverter extends ChoiceConverter

trait CatsCoflatMap[F[_]] extends CatsFunctor[F] {
  self: cats.CoflatMap[F] =>

  protected implicit val scalazCobind: scalaz.Cobind[F]
  override protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F] = scalazCobind
  override protected implicit val scalazFunctor: scalaz.Functor[F] = scalazCobind

  override def coflatMap[A, B](fa: F[A])(f: (F[A]) => B): F[B] =
    scalazCobind.cobind(fa)((a: F[A]) => f(a))

}

trait CoBindConverter {

  implicit def scalazToCatsCoFlatMapInstance[F[_]](implicit inner: scalaz.Cobind[F]): cats.CoflatMap[F] =
    new CatsCoflatMap[F] with cats.CoflatMap[F] {
      override protected implicit val scalazCobind: scalaz.Cobind[F] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsCoFlatMapValue[F[_]](inner: scalaz.Cobind[F]): cats.CoflatMap[F] =
    scalazToCatsCoFlatMapInstance[F](inner)

}

object CoBindConverter extends CoBindConverter

trait CatsComonad[F[_]] extends CatsCoflatMap[F] {
  self: cats.Comonad[F] =>

  protected implicit val scalazComonad: scalaz.Comonad[F]
  override protected implicit val scalazCobind: scalaz.Cobind[F] = scalazComonad

  override def extract[A](x: F[A]): A =
    scalazComonad.copoint(x)

}

trait ComonadConverter {

  implicit def scalazToCatsComonadInstance[F[_]](implicit inner: scalaz.Comonad[F]): cats.Comonad[F] =
    new CatsComonad[F] with cats.Comonad[F] {
      override protected implicit val scalazComonad: scalaz.Comonad[F] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsComonadValue[F[_]](inner: scalaz.Comonad[F]): cats.Comonad[F] =
    scalazToCatsComonadInstance[F](inner)

}

object ComonadConverter extends ComonadConverter

trait CatsCompose[F[_, _]] {
  self: cats.arrow.Compose[F] =>

  protected implicit val scalazCompose: scalaz.Compose[F]

  override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
    scalazCompose.compose(f, g)

}

trait ComposeConverter {

  implicit def scalazToCatsComposeInstance[F[_, _]](implicit inner: scalaz.Compose[F]): cats.arrow.Compose[F] =
    new CatsCompose[F] with cats.arrow.Compose[F] {
      override protected implicit val scalazCompose: scalaz.Compose[F] = inner
    }

  implicit def scalazToCatsComposeValue[F[_, _]](inner: scalaz.Compose[F]): cats.arrow.Compose[F] =
    scalazToCatsComposeInstance[F](inner)

}

object ComposeConverter extends ComposeConverter

trait CatsContravariant[F[_]] {
  self: cats.functor.Contravariant[F] =>

  protected implicit val scalazContravariant: scalaz.Contravariant[F]

  override def contramap[A, B](fa: F[A])(f: (B) => A): F[B] =
    scalazContravariant.contramap(fa)(f)

}

trait ContravariantConverter {

  implicit def scalazToCatsContravariantInstance[F[_]](implicit inner: scalaz.Contravariant[F]): cats.functor.Contravariant[F] =
    new CatsContravariant[F] with cats.functor.Contravariant[F] {
      override protected implicit val scalazContravariant: scalaz.Contravariant[F] = inner
    }

  implicit def scalazToCatsContravariantValue[F[_]](inner: scalaz.Contravariant[F]): cats.functor.Contravariant[F] =
    scalazToCatsContravariantInstance[F](inner)

}

object ContravariantConverter extends ContravariantConverter

trait CatsEq[F] {
  self: cats.Eq[F] =>

  protected implicit def scalazEqual: scalaz.Equal[F]

  override def eqv(x: F, y: F): Boolean =
    scalazEqual.equal(x, y)
}

trait EqualConverter {

  implicit def catsToScalazEqual[F](implicit inner: scalaz.Equal[F]): cats.Eq[F] =
    new CatsEq[F] with cats.Eq[F] {
      override protected implicit val scalazEqual: scalaz.Equal[F] = inner
    }

}

object EqualConverter extends EqualConverter

trait CatsFoldable[F[_]] {
  self: cats.Foldable[F] =>

  import cats.Eval

  protected implicit val scalazFoldable: scalaz.Foldable[F]

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    scalazFoldable.foldLeft(fa, b)(f)

  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def catsF(a: A, b: => Eval[B]): Eval[B] =
      f(a, b)

    scalazFoldable.foldRight[A, Eval[B]](fa, lb)(catsF)
  }
}

trait FoldableConverter {

  implicit def scalazToCatsFoldableInstance[F[_]](implicit inner: scalaz.Foldable[F]): cats.Foldable[F] =
    new CatsFoldable[F] with cats.Foldable[F] {
      override protected implicit val scalazFoldable: scalaz.Foldable[F] = inner
    }

  implicit def scalazToCatsFoldableValue[F[_]](inner: scalaz.Foldable[F]): cats.Foldable[F] =
    scalazToCatsFoldableInstance[F](inner)
}

object FoldableConverter extends FoldableConverter

trait CatsFunctor[F[_]] extends CatsInvariantFunctor[F] {
  self: cats.Functor[F] =>

  protected implicit val scalazFunctor: scalaz.Functor[F]
  override protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F] = scalazFunctor

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] =
    scalazFunctor.map(fa)(f)

}

trait FunctorConverter {

  implicit def scalazToCatsFunctorInstance[F[_]](implicit inner: scalaz.Functor[F]): cats.Functor[F] =
    new CatsFunctor[F] with cats.Functor[F] {
      override protected implicit val scalazFunctor: scalaz.Functor[F] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsFunctorValue[F[_]](inner: scalaz.Functor[F]): cats.Functor[F] =
    scalazToCatsFunctorInstance[F](inner)

}

object FunctorConverter extends FunctorConverter

trait CatsInvariantFunctor[F[_]] {
  self: cats.functor.Invariant[F] =>

  protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F]

  override def imap[A, B](fa: F[A])(f: (A) => B)(g: (B) => A): F[B] =
    CatsInvariantFunctor.imap(fa)(f, g)(scalazInvariantFunctor)
}

object CatsInvariantFunctor {
  def imap[F[_], A, B](fa: F[A])(f: (A) => B, g: (B) => A)(implicit inner: scalaz.InvariantFunctor[F]): F[B] =
    inner.xmap(fa, f, g)
}

trait InvariantFunctorConverter {

  implicit def scalazToCatsInvariantInstance[F[_]](implicit inner: scalaz.InvariantFunctor[F]): cats.functor.Invariant[F] =
    new CatsInvariantFunctor[F] with cats.functor.Invariant[F] {
      override protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F] = inner
    }

  implicit def scalazToCatsInvariantValue[F[_]](inner: scalaz.InvariantFunctor[F]): cats.functor.Invariant[F] =
    scalazToCatsInvariantInstance[F](inner)

}

object InvariantFunctorConverter extends InvariantFunctorConverter

trait CatsMonad[F[_]]
  extends CatsBind[F]
    with CatsApplicative[F] {
  self: cats.Monad[F] =>

  protected implicit val scalazMonad: scalaz.Monad[F]
  override protected implicit val scalazApplicative: scalaz.Applicative[F] = scalazMonad
}

trait MonadConverter {

  implicit def scalazToCatsMonadInstance[F[_]](implicit inner: scalaz.Monad[F], inner0: scalaz.BindRec[F]): cats.Monad[F] =
    new CatsMonad[F] with cats.Monad[F] {
      override protected implicit val scalazBindRec = inner0
      override protected implicit val scalazMonad: scalaz.Monad[F] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsMonadValue[F[_]](inner: scalaz.Monad[F])(implicit inner0: scalaz.BindRec[F]): cats.Monad[F] =
    scalazToCatsMonadInstance[F](inner, inner0)

}

object MonadConverter extends MonadConverter

trait CatsMonadError[F[_], E] extends CatsMonad[F] {
  self: cats.MonadError[F, E] =>

  protected implicit val scalazMonadError: scalaz.MonadError[F, E]
  override protected implicit val scalazMonad: scalaz.Monad[F] = scalazMonadError

  override def raiseError[A](e: E): F[A] =
    scalazMonadError.raiseError(e)

  override def handleErrorWith[A](fa: F[A])(f: (E) => F[A]): F[A] =
    scalazMonadError.handleError(fa)(e => f(e))

  override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
    CatsInvariantFunctor.imap(fa)(f, fi)

}

trait MonadErrorConverter {

  implicit def scalazToCatsMonadErrorInstance[F[_], E](implicit inner: scalaz.MonadError[F, E], inner0: scalaz.BindRec[F]): cats.MonadError[F, E] =
    new CatsMonadError[F, E] with cats.MonadError[F, E] {

      override protected implicit val scalazBindRec = inner0
      override protected implicit val scalazMonadError: scalaz.MonadError[F, E] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsMonadErrorValue[F[_], E](inner: scalaz.MonadError[F, E])(implicit inner0: scalaz.BindRec[F]): cats.MonadError[F, E] =
    scalazToCatsMonadErrorInstance[F, E](inner, inner0)

}

object MonadErrorConverter extends MonadErrorConverter

trait CatsMonadReader[F[_], S] extends CatsMonad[F] {
  self: cats.MonadReader[F, S] =>

  protected implicit val scalazMonadReader: scalaz.MonadReader[F, S]
  override protected implicit val scalazMonad: scalaz.Monad[F] = scalazMonadReader

  override def ask: F[S] =
    scalazMonadReader.ask

  override def local[A](f: (S) => S)(fa: F[A]): F[A] =
    scalazMonadReader.local(f)(fa)

}

trait MonadReaderConverter {

  implicit def scalazToCatsMonadReaderInstance[F[_], S](implicit inner: scalaz.MonadReader[F, S], inner0: scalaz.BindRec[F]): cats.MonadReader[F, S] =
    new CatsMonadReader[F, S] with cats.MonadReader[F, S] {
      override protected implicit val scalazMonadReader: scalaz.MonadReader[F, S] = inner
      override protected implicit val scalazBindRec: scalaz.BindRec[F] = inner0

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsMonadReaderValue[F[_], S](inner: scalaz.MonadReader[F, S])(implicit inner0: scalaz.BindRec[F]): cats.MonadReader[F, S] =
    scalazToCatsMonadReaderInstance[F, S](inner, inner0)

}

object MonadReaderConverter extends MonadReaderConverter

trait CatsMonadState[F[_], S]
  extends CatsMonad[F] {
  self: cats.MonadState[F, S] =>

  protected implicit val scalazMonadState: scalaz.MonadState[F, S]
  override protected implicit val scalazMonad: scalaz.Monad[F] = scalazMonadState

  override def get: F[S] =
    scalazMonadState.get

  override def set(s: S): F[Unit] =
    scalazMonadState.put(s)

}

trait MonadStateConverter {

  implicit def scalazToCatsMonadStateInstance[F[_], S](implicit inner: scalaz.MonadState[F, S], inner0: scalaz.BindRec[F]): cats.Monad[F] =
    new CatsMonadState[F, S] with cats.MonadState[F, S] {
      override protected implicit val scalazMonadState: scalaz.MonadState[F, S] = inner
      override protected implicit val scalazBindRec: scalaz.BindRec[F] = inner0

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsMonadStateValue[F[_], S](inner: scalaz.MonadState[F, S])(implicit inner0: scalaz.BindRec[F]): cats.Monad[F] =
    scalazToCatsMonadStateInstance[F, S](inner, inner0)
}

object MonadStateConverter extends MonadStateConverter

trait CatsMonoid[F] {
  self: cats.Monoid[F] =>

  protected implicit val scalazMonoid: scalaz.Monoid[F]

  override def empty: F =
    scalazMonoid.zero

  override def combine(x: F, y: F): F =
    scalazMonoid.append(x, y)
}

trait MonoidConverter {

  implicit def scalazToCatsMonoidInstance[F](implicit inner: scalaz.Monoid[F]): cats.Monoid[F] =
    new CatsMonoid[F] with cats.Monoid[F] {
      override protected implicit val scalazMonoid: scalaz.Monoid[F] = inner
    }

  implicit def scalazToCatsMonoidValue[F](inner: scalaz.Monoid[F]): cats.Monoid[F] =
    scalazToCatsMonoidInstance[F](inner)

}

object MonoidConverter extends MonoidConverter

trait CatsOrder[F] {
  self: cats.Order[F] =>

  protected implicit val scalazOrder: scalaz.Order[F]

  override def compare(x: F, y: F): Int =
    scalazOrder.order(x, y).toInt
}

trait OrderConverter {

  implicit def scalazToCatsOrderInstance[F](implicit inner: scalaz.Order[F]): cats.Order[F] =
    new CatsOrder[F] with cats.Order[F] {
      override protected implicit val scalazOrder: scalaz.Order[F] = inner
    }

  implicit def scalazToCatsValue[F](inner: scalaz.Order[F]): cats.Order[F] =
    scalazToCatsOrderInstance[F](inner)

}

object OrderConverter extends OrderConverter

trait CatsNaturalTransformation[F[_], G[_]] {
  self: cats.arrow.FunctionK[F, G] =>

  protected implicit val scalazNaturalTransformation: scalaz.NaturalTransformation[F, G]

  override def apply[A](fa: F[A]): G[A] =
    scalazNaturalTransformation.apply(fa)

}

trait NaturalTransformationConverter {

  implicit def scalazToCatsFunctionKInstance[F[_], G[_]](implicit inner: scalaz.NaturalTransformation[F, G]): cats.arrow.FunctionK[F, G] =
    new CatsNaturalTransformation[F, G] with cats.arrow.FunctionK[F, G] {
      override protected implicit val scalazNaturalTransformation: scalaz.NaturalTransformation[F, G] = inner
    }

  implicit def scalazToCatsFunctionKValue[F[_], G[_]](inner: scalaz.NaturalTransformation[F, G]): cats.arrow.FunctionK[F, G] =
    scalazToCatsFunctionKInstance[F, G](inner)

}

object NaturalTransformationConverter extends NaturalTransformationConverter

trait CatsShow[F] {
  self: cats.Show[F] =>

  protected implicit val scalazShow: scalaz.Show[F]

  override def show(f: F): String =
    scalazShow.shows(f)
}

trait ShowConverter {

  implicit def scalazToCatsShowInstance[F](implicit inner: scalaz.Show[F]): cats.Show[F] =
    new CatsShow[F] with cats.Show[F] {
      override protected implicit val scalazShow: scalaz.Show[F] = inner
    }

  implicit def scalazToCatsShowValue[F](implicit inner: scalaz.Show[F]): cats.Show[F] =
    scalazToCatsShowInstance[F](inner)

}

object ShowConverter extends ShowConverter

trait CatsTraverse[F[_]] extends CatsFunctor[F] with CatsFoldable[F] {
  self: cats.Traverse[F] =>

  import harmony.toscalaz.typeclass.ApplicativeConverter._

  protected implicit val scalazTraverse: scalaz.Traverse[F]
  override protected implicit val scalazFunctor: scalaz.Functor[F] = scalazTraverse
  override protected implicit val scalazFoldable: scalaz.Foldable[F] = scalazTraverse

  override def traverse[T[_], A, B](fa: F[A])(f: (A) => T[B])(implicit a: cats.Applicative[T]): T[F[B]] = {
    scalazTraverse.traverse(fa)(f)
  }

}

trait TraverseConverter {

  implicit def scalazToCatsTraverseInstance[F[_]](implicit inner: scalaz.Traverse[F]): cats.Traverse[F] =
    new CatsTraverse[F] with cats.Traverse[F] {
      override protected implicit val scalazTraverse: scalaz.Traverse[F] = inner

      override def imap[A, B](fa: F[A])(f: (A) => B)(fi: (B) => A): F[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor)
    }

  implicit def scalazToCatsTraverseValue[F[_]](inner: scalaz.Traverse[F]): cats.Traverse[F] =
    scalazToCatsTraverseInstance[F](inner)

}

object TraverseConverter extends TraverseConverter
