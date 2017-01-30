package harmony.tocats.typeclass

import harmony._

trait CatsAlternative[F[_], F0[_]] extends CatsApplicative[F, F0] {
  self: cats.Alternative[F0] =>

  protected implicit val scalazApplicativePlus: scalaz.ApplicativePlus[F]
  override protected implicit val scalazApplicative: scalaz.Applicative[F] = scalazApplicativePlus

  override def empty[A]: F0[A] =
    trans(scalazApplicativePlus.empty[A])

  override def combineK[A](x: F0[A], y: F0[A]): F0[A] =
    trans(scalazApplicativePlus.plus(trans.reverse(x), trans.reverse(y)))

}

trait AlternativeConverter {

  implicit def scalazToCatsAlternative[F[_], F0[_]](implicit inner: scalaz.ApplicativePlus[F], t: ReversableNatTrans[F, F0]): cats.Alternative[F0] =
    new CatsAlternative[F, F0] with cats.Alternative[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazApplicativePlus: scalaz.ApplicativePlus[F] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, t)
    }

}

object AlternativeConverter extends AlternativeConverter

trait CatsApplicative[F[_], F0[_]] extends CatsApply[F, F0] {
  self: cats.Applicative[F0] =>

  protected implicit val scalazApplicative: scalaz.Applicative[F]
  override protected implicit val scalazApply: scalaz.Apply[F] = scalazApplicative

  override def ap[A, B](ff: F0[(A) => B])(fa: F0[A]): F0[B] =
    trans(scalazApplicative.ap(trans.reverse(fa))(trans.reverse(ff)))

  override def pure[A](x: A): F0[A] =
    trans(scalazApplicative.point(x))

}

trait ApplicativeConverter {

  implicit def scalazToCatsApplicative[F[_], F0[_]](implicit inner: scalaz.Applicative[F], t: ReversableNatTrans[F, F0]): cats.Applicative[F0] =
    new CatsApplicative[F, F0] with cats.Applicative[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazApplicative: scalaz.Applicative[F] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object ApplicativeConverter extends ApplicativeConverter

trait CatsArrow[F[_, _], F0[_, _]] {
  self: cats.arrow.Arrow[F0] =>

  protected implicit val trans: ReversableBiNatTrans[F, F0]
  protected implicit val scalazArrow: scalaz.Arrow[F]

  override def lift[A, B](f: (A) => B): F0[A, B] =
    trans(scalazArrow.arr(f))


  override def first[A, B, C](fa: F0[A, B]): F0[(A, C), (B, C)] =
    trans(scalazArrow.first(trans.reverse(fa)))

  override def id[A]: F0[A, A] =
    trans(scalazArrow.id[A])

  override def compose[A, B, C](f: F0[B, C], g: F0[A, B]): F0[A, C] =
    trans(scalazArrow.compose(trans.reverse(f), trans.reverse(g)))
}

trait ArrowConverter {

  implicit def scalazToCatsArrow[F[_, _], F0[_, _]](implicit inner: scalaz.Arrow[F], t: ReversableBiNatTrans[F, F0]): cats.arrow.Arrow[F0] =
    new CatsArrow[F, F0] with cats.arrow.Arrow[F0] {
      override protected implicit val trans: ReversableBiNatTrans[F, F0] = t
      override protected implicit val scalazArrow: scalaz.Arrow[F] = inner
    }

}

object ArrowConverter extends ArrowConverter

trait CatsApply[F[_], F0[_]] extends CatsFunctor[F, F0] {
  self: cats.Apply[F0] =>

  protected implicit val scalazApply: scalaz.Apply[F]
  override protected implicit val scalazFunctor: scalaz.Functor[F] = scalazApply

  override def ap[A, B](ff: F0[(A) => B])(fa: F0[A]): F0[B] =
    trans(scalazApply.ap(trans.reverse(fa))(trans.reverse(ff)))

}

trait ApplyConverter {

  implicit def scalazToCatsApply[F[_], F0[_]](implicit inner: scalaz.Apply[F], t: ReversableNatTrans[F, F0]): cats.Apply[F0] =
    new CatsApply[F, F0] with cats.Apply[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazApply: scalaz.Apply[F] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object ApplyConverter extends ApplyConverter

trait CatsBifoldable[F[_, _], F0[_, _]] {
  self: cats.Bifoldable[F0] =>

  import cats.Eval

  protected implicit val scalazBifoldable: scalaz.Bifoldable[F]
  protected implicit val trans: BiNaturalTransformation[F0, F]

  override def bifoldLeft[A, B, C](fab: F0[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    scalazBifoldable.bifoldLeft(trans(fab), c)(f)(g)

  override def bifoldRight[A, B, C](
    fab: F0[A, B],
    c: Eval[C]
  )(f: (A, Eval[C]) => Eval[C],
    g: (B, Eval[C]) => Eval[C]
  ): Eval[C] =
    //TODO: is this a correct evaluation laziness?
    Eval.later[C] {
      scalazBifoldable.bifoldRight[A, B, C](
        trans(fab),
        c.value
      )((a, c) => f(a, Eval.now(c)).value
      )((b, c) => g(b, Eval.now(c)).value
      )
    }

}

trait BifoldableConverter {

  implicit def scalazToCatsBifoldable[F[_, _], F0[_, _]](implicit inner: scalaz.Bifoldable[F], t: BiNaturalTransformation[F0, F]): cats.Bifoldable[F0] =
    new CatsBifoldable[F, F0] with cats.Bifoldable[F0] {
      override protected implicit val scalazBifoldable: scalaz.Bifoldable[F] = inner
      override protected implicit val trans: BiNaturalTransformation[F0, F] = t
    }

}

object BifoldableConverter extends BifoldableConverter

trait CatsBifunctor[F[_, _], F0[_, _]] {
  self: cats.functor.Bifunctor[F0] =>

  protected implicit val scalazBifunctor: scalaz.Bifunctor[F]
  protected implicit val trans: ReversableBiNatTrans[F, F0]

  override def bimap[A, B, C, D](fab: F0[A, B])(f: (A) => C, g: (B) => D): F0[C, D] =
    trans(scalazBifunctor.bimap(trans.reverse(fab))(f, g))

}

trait BifunctorConverter {

  implicit def scalazToCatsBifunctor[F[_, _], F0[_, _]](implicit inner: scalaz.Bifunctor[F], t: ReversableBiNatTrans[F, F0]): cats.functor.Bifunctor[F0] =
    new CatsBifunctor[F, F0] with cats.functor.Bifunctor[F0] {
      override protected implicit val scalazBifunctor: scalaz.Bifunctor[F] = inner
      override protected implicit val trans: ReversableBiNatTrans[F, F0] = t
    }

}

object BifunctorConverter extends BifunctorConverter

trait CatsBind[F[_], F0[_]] extends CatsApply[F, F0] {
  self: cats.FlatMap[F0] =>

  protected implicit val scalazBindRec: scalaz.BindRec[F]
  override protected implicit val scalazApply: scalaz.Apply[F] = scalazBindRec

  override def flatMap[A, B](fa: F0[A])(f: (A) => F0[B]): F0[B] =
    trans(scalazBindRec.bind(trans.reverse(fa))(a => trans.reverse(f(a))))

  override def tailRecM[A, B](a: A)(f: (A) => F0[Either[A, B]]): F0[B] =
    trans(scalazBindRec.tailrecM((a: A) => scalazBindRec.map(trans.reverse(f(a)))(scalaz.Disjunction.fromEither))(a))

}

trait BindConverter {

  implicit def scalazToCatsFlatMap[F[_], F0[_]](implicit inner: scalaz.BindRec[F], t: ReversableNatTrans[F, F0]): cats.FlatMap[F0] =
    new CatsBind[F, F0] with cats.FlatMap[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazBindRec: scalaz.BindRec[F] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object BindConverter extends BindConverter

trait CatsCategory[F[_, _], F0[_, _]] {
  self: cats.arrow.Category[F0] =>

  protected implicit val scalazCategory: scalaz.Category[F]
  protected implicit val trans: ReversableBiNatTrans[F, F0]

  override def id[A]: F0[A, A] =
    trans(scalazCategory.id[A])

  override def compose[A, B, C](f: F0[B, C], g: F0[A, B]): F0[A, C] =
    trans(scalazCategory.compose(trans.reverse(f), trans.reverse(g)))

}

trait CategoryConverter {

  implicit def scalazToCatsCategory[F[_, _], F0[_, _]](implicit inner: scalaz.Category[F], t: ReversableBiNatTrans[F, F0]): cats.arrow.Category[F0] =
    new CatsCategory[F, F0] with cats.arrow.Category[F0] {
      override protected implicit val scalazCategory: scalaz.Category[F] = inner
      override protected implicit val trans: ReversableBiNatTrans[F, F0] = t
    }

  implicit def scalazToCatsCategoryValue[F[_, _], F0[_, _]](inner: scalaz.Category[F])(implicit t: ReversableBiNatTrans[F, F0]): cats.arrow.Category[F0] =
    scalazToCatsCategory[F, F0](inner, t)

}

object CategoryConverter extends CategoryConverter

trait CatsChoice[F[_, _], F0[_, _]] {
  self: cats.arrow.Choice[F0] =>

  protected implicit val trans: ReversableBiNatTrans[F, F0]
  protected implicit val scalazChoice: scalaz.Choice[F]

  override def choice[A, B, C](f: F0[A, C], g: F0[B, C]): F0[Either[A, B], C] =
    ???

  override def id[A]: F0[A, A] =
    trans(scalazChoice.id[A])

  override def compose[A, B, C](f: F0[B, C], g: F0[A, B]): F0[A, C] =
    trans(scalazChoice.compose(trans.reverse(f), trans.reverse(g)))
}

trait ChoiceConverter {

  implicit def scalazToCatsChoice[F[_, _], F0[_, _]](implicit inner: scalaz.Choice[F], t: ReversableBiNatTrans[F, F0]): cats.arrow.Choice[F0] =
    new CatsChoice[F, F0] with cats.arrow.Choice[F0] {
      override protected implicit val trans: ReversableBiNatTrans[F, F0] = t
      override protected implicit val scalazChoice: scalaz.Choice[F] = inner
    }

}

object ChoiceConverter extends ChoiceConverter

trait CatsCoflatMap[F[_], F0[_]] extends CatsFunctor[F, F0] {
  self: cats.CoflatMap[F0] =>

  protected implicit val scalazCobind: scalaz.Cobind[F]
  override protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F] = scalazCobind
  override protected implicit val scalazFunctor: scalaz.Functor[F] = scalazCobind

  override def coflatMap[A, B](fa: F0[A])(f: (F0[A]) => B): F0[B] =
    trans(scalazCobind.cobind(trans.reverse(fa))((a: F[A]) => f(trans(a))))

}

trait CoBindConverter {

  implicit def scalazToCatsCoFlatMap[F[_], F0[_]](implicit inner: scalaz.Cobind[F], t: ReversableNatTrans[F, F0]): cats.CoflatMap[F0] =
    new CatsCoflatMap[F, F0] with cats.CoflatMap[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazCobind: scalaz.Cobind[F] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object CoBindConverter extends CoBindConverter

trait CatsComonad[F[_], F0[_]] extends CatsCoflatMap[F, F0] {
  self: cats.Comonad[F0] =>

  protected implicit val scalazComonad: scalaz.Comonad[F]
  override protected implicit val scalazCobind: scalaz.Cobind[F] = scalazComonad

  override def extract[A](x: F0[A]): A =
    scalazComonad.copoint(trans.reverse(x))

}

trait ComonadConverter {

  implicit def scalazToCatsComonad[F[_], F0[_]](implicit inner: scalaz.Comonad[F], t: ReversableNatTrans[F, F0]): cats.Comonad[F0] =
    new CatsComonad[F, F0] with cats.Comonad[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazComonad: scalaz.Comonad[F] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object ComonadConverter extends ComonadConverter

trait CatsCompose[F[_, _], F0[_, _]] {
  self: cats.arrow.Compose[F0] =>

  protected implicit val trans: ReversableBiNatTrans[F, F0]
  protected implicit val scalazCompose: scalaz.Compose[F]

  override def compose[A, B, C](f: F0[B, C], g: F0[A, B]): F0[A, C] =
    trans(scalazCompose.compose(trans.reverse(f), trans.reverse(g)))

}

trait ComposeConverter {

  implicit def scalazToCatsCompose[F[_, _], F0[_, _]](implicit inner: scalaz.Compose[F], t: ReversableBiNatTrans[F, F0]): cats.arrow.Compose[F0] =
    new CatsCompose[F, F0] with cats.arrow.Compose[F0] {
      override protected implicit val trans: ReversableBiNatTrans[F, F0] = t
      override protected implicit val scalazCompose: scalaz.Compose[F] = inner
    }

}

object ComposeConverter extends ComposeConverter

trait CatsContravariant[F[_], F0[_]] {
  self: cats.functor.Contravariant[F0] =>

  protected implicit val scalazContravariant: scalaz.Contravariant[F]
  protected implicit val trans: ReversableNatTrans[F, F0]

  override def contramap[A, B](fa: F0[A])(f: (B) => A): F0[B] =
    trans(scalazContravariant.contramap(trans.reverse(fa))(f))

}

trait ContravariantConverter {

  implicit def scalazToCatsContravariant[F[_], F0[_]](implicit inner: scalaz.Contravariant[F], t: ReversableNatTrans[F, F0]): cats.functor.Contravariant[F0] =
    new CatsContravariant[F, F0] with cats.functor.Contravariant[F0] {
      override protected implicit val scalazContravariant: scalaz.Contravariant[F] = inner
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
    }

}

object ContravariantConverter extends ContravariantConverter

trait CatsEq[F, F0] {
  self: cats.Eq[F0] =>

  protected implicit val scalazEqual: scalaz.Equal[F]
  protected implicit def f0(f: F): F0
  protected implicit def f(f0: F0): F

  override def eqv(x: F0, y: F0): Boolean =
    scalazEqual.equal(x, y)
}

trait EqualConverter {

  implicit def catsToScalazEqual[F, F0](implicit inner: scalaz.Equal[F], toF0: F => F0, toF: F0 => F): cats.Eq[F0] =
    new CatsEq[F, F0] with cats.Eq[F0] {
      override protected implicit val scalazEqual: scalaz.Equal[F] = inner

      override protected implicit def f0(f: F): F0 = toF0(f)

      override protected implicit def f(f0: F0): F = toF(f0)
    }

}

object EqualConverter extends EqualConverter

trait CatsFoldable[F[_], F0[_]] {
  self: cats.Foldable[F0] =>

  import cats.Eval

  protected implicit val revTrans: NaturalTransformation[F0, F]
  protected implicit val scalazFoldable: scalaz.Foldable[F]

  override def foldLeft[A, B](fa: F0[A], b: B)(f: (B, A) => B): B =
    scalazFoldable.foldLeft(revTrans(fa), b)(f)

  override def foldRight[A, B](fa: F0[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def catsF(a: A, b: => Eval[B]): Eval[B] =
      f(a, b)

    scalazFoldable.foldRight[A, Eval[B]](revTrans(fa), lb)(catsF)
  }
}

trait FoldableConverter {

  implicit def scalazToCatsFoldable[F[_], F0[_]](implicit inner: scalaz.Foldable[F], t: NaturalTransformation[F0, F]): cats.Foldable[F0] =
    new CatsFoldable[F, F0] with cats.Foldable[F0] {
      override protected implicit val revTrans: NaturalTransformation[F0, F] = t
      override protected implicit val scalazFoldable: scalaz.Foldable[F] = inner
    }

  implicit def scalazToCatsFoldableId[F[_]](implicit inner: scalaz.Foldable[F]): cats.Foldable[F] = {
    implicit val t = NaturalTransformation.refl[F]
    scalazToCatsFoldable[F, F]
  }

}

object FoldableConverter extends FoldableConverter

trait CatsFunctor[F[_], F0[_]] extends CatsInvariantFunctor[F, F0] {
  self: cats.Functor[F0] =>

  protected implicit val scalazFunctor: scalaz.Functor[F]
  override protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F] = scalazFunctor

  override def map[A, B](fa: F0[A])(f: (A) => B): F0[B] =
    trans(scalazFunctor.map(trans.reverse(fa))(f))

}

trait FunctorConverter {

  implicit def scalazToCatsFunctor[F[_], F0[_]](implicit inner: scalaz.Functor[F], t: ReversableNatTrans[F, F0]): cats.Functor[F0] =
    new CatsFunctor[F, F0] with cats.Functor[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazFunctor: scalaz.Functor[F] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

//  implicit def scalazToCatsFunctorId[F[_]](implicit inner: scalaz.Functor[F]): cats.Functor[F] =
//    scalazToCatsFunctor[F, F]

  implicit def scalazToCatsFunctorValue[F[_], F0[_]](inner: scalaz.Functor[F])(implicit t: ReversableNatTrans[F, F0]): cats.Functor[F0] =
    scalazToCatsFunctor[F, F0](inner, t)

}

object FunctorConverter extends FunctorConverter

trait CatsInvariantFunctor[F[_], F0[_]] {
  self: cats.functor.Invariant[F0] =>

  protected implicit val trans: ReversableNatTrans[F, F0]
  protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F]

  override def imap[A, B](fa: F0[A])(f: (A) => B)(g: (B) => A): F0[B] =
    CatsInvariantFunctor.imap(fa)(f, g)
}

object CatsInvariantFunctor {
  def imap[F[_], F0[_], A, B](fa: F0[A])(f: (A) => B, g: (B) => A)(implicit inner: scalaz.InvariantFunctor[F], t: ReversableNatTrans[F, F0]): F0[B] =
    t(inner.xmap(t.reverse(fa), f, g))
}

trait InvariantFunctorConverter {

  implicit def scalazToCatsInvariantInstance[F[_], F0[_]](implicit inner: scalaz.InvariantFunctor[F], t: ReversableNatTrans[F, F0]): cats.functor.Invariant[F0] =
    new CatsInvariantFunctor[F, F0] with cats.functor.Invariant[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F] = inner
    }

  implicit def scalazToCatsInvariantValue[F[_], F0[_]](inner: scalaz.InvariantFunctor[F])(implicit t: ReversableNatTrans[F, F0]): cats.functor.Invariant[F0] =
    scalazToCatsInvariantInstance[F, F0](inner, t)

}

object InvariantFunctorConverter extends InvariantFunctorConverter

trait CatsMonad[F[_], F0[_]]
  extends CatsBind[F, F0]
    with CatsApplicative[F, F0] {
  self: cats.Monad[F0] =>

  protected implicit val scalazMonad: scalaz.Monad[F]
  override protected implicit val scalazApplicative: scalaz.Applicative[F] = scalazMonad
}

trait MonadConverter {

  implicit def scalazToCatsMonad[F[_], F0[_]](implicit inner: scalaz.Monad[F], inner0: scalaz.BindRec[F], t: ReversableNatTrans[F, F0]): cats.Monad[F0] =
    new CatsMonad[F, F0] with cats.Monad[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazBindRec = inner0
      override protected implicit val scalazMonad: scalaz.Monad[F] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object MonadConverter extends MonadConverter

trait CatsMonadError[F[_], F0[_], E] extends CatsMonad[F, F0] {
  self: cats.MonadError[F0, E] =>

  protected implicit val scalazMonadError: scalaz.MonadError[F, E]
  override protected implicit val scalazMonad: scalaz.Monad[F] = scalazMonadError

  override def raiseError[A](e: E): F0[A] =
    trans(scalazMonadError.raiseError(e))

  override def handleErrorWith[A](fa: F0[A])(f: (E) => F0[A]): F0[A] =
    trans(scalazMonadError.handleError(trans.reverse(fa))(e => trans.reverse(f(e))))

  override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
    CatsInvariantFunctor.imap(fa)(f, fi)

}

trait MonadErrorConverter {

  implicit def scalazToCatsMonadError[F[_], F0[_], E](implicit inner: scalaz.MonadError[F, E], inner0: scalaz.BindRec[F], t: ReversableNatTrans[F, F0]): cats.MonadError[F0, E] =
    new CatsMonadError[F, F0, E] with cats.MonadError[F0, E] {

      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazBindRec = inner0
      override protected implicit val scalazMonadError: scalaz.MonadError[F, E] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object MonadErrorConverter extends MonadErrorConverter

trait CatsMonadReader[F[_], F0[_], S] extends CatsMonad[F, F0] {
  self: cats.MonadReader[F0, S] =>

  protected implicit val scalazMonadReader: scalaz.MonadReader[F, S]
  override protected implicit val scalazMonad: scalaz.Monad[F] = scalazMonadReader

  override def ask: F0[S] =
    trans(scalazMonadReader.ask)

  override def local[A](f: (S) => S)(fa: F0[A]): F0[A] =
    trans(scalazMonadReader.local(f)(trans.reverse(fa)))

}

trait MonadReaderConverter {

  implicit def scalazToCatsMonadReader[F[_], F0[_], S](implicit inner: scalaz.MonadReader[F, S], inner0: scalaz.BindRec[F], t: ReversableNatTrans[F, F0]): cats.MonadReader[F0, S] =
    new CatsMonadReader[F, F0, S] with cats.MonadReader[F0, S] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazMonadReader: scalaz.MonadReader[F, S] = inner
      override protected implicit val scalazBindRec: scalaz.BindRec[F] = inner0

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(inner, trans)
    }

}

object MonadReaderConverter extends MonadReaderConverter

trait CatsMonadState[F[_], F0[_], S]
  extends CatsMonad[F, F0] {
  self: cats.MonadState[F0, S] =>

  protected implicit val scalazMonadState: scalaz.MonadState[F, S]
  override protected implicit val scalazMonad: scalaz.Monad[F] = scalazMonadState

  override def get: F0[S] =
    trans(scalazMonadState.get)

  override def set(s: S): F0[Unit] =
    trans(scalazMonadState.put(s))

}

trait MonadStateConverter {

  implicit def scalazToCatsMonadState[F[_], F0[_], S](implicit inner: scalaz.MonadState[F, S], inner0: scalaz.BindRec[F], t: ReversableNatTrans[F, F0]): cats.Monad[F0] =
    new CatsMonadState[F, F0, S] with cats.MonadState[F0, S] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazMonadState: scalaz.MonadState[F, S] = inner
      override protected implicit val scalazBindRec: scalaz.BindRec[F] = inner0

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(inner, trans)
    }

}

object MonadStateConverter extends MonadStateConverter

trait CatsMonoid[F, F0] {
  self: cats.Monoid[F0] =>

  protected implicit val scalazMonoid: scalaz.Monoid[F]
  protected implicit def trans(f: F): F0
  protected implicit def reverse(f0: F0): F

  override def empty: F0 =
    trans(scalazMonoid.zero)

  override def combine(x: F0, y: F0): F0 =
    scalazMonoid.append(x, y)
}

trait MonoidConverter {

  implicit def scalazToCatsMonoid[F, F0](implicit inner: scalaz.Monoid[F], t: F => F0, r: F0 => F): cats.Monoid[F0] =
    new CatsMonoid[F, F0] with cats.Monoid[F0] {
      override protected implicit val scalazMonoid: scalaz.Monoid[F] = inner

      override protected implicit def trans(f: F): F0 = t(f)

      override protected implicit def reverse(f0: F0): F = r(f0)
    }

}

object MonoidConverter extends MonoidConverter

trait CatsOrder[F] {
  self: cats.Order[F] =>

  protected implicit val scalazOrder: scalaz.Order[F]

  override def compare(x: F, y: F): Int =
    scalazOrder.order(x, y).toInt
}

trait CatsNaturalTransformation[F[_], G[_]] {
  self: cats.arrow.FunctionK[F, G] =>

  protected implicit val scalazNaturalTransformation: scalaz.NaturalTransformation[F, G]

  override def apply[A](fa: F[A]): G[A] =
    scalazNaturalTransformation.apply(fa)

}

trait NaturalTransformationConverter {

  implicit def scalazToCatsFunctionK[F[_], G[_]](implicit inner: scalaz.NaturalTransformation[F, G]): cats.arrow.FunctionK[F, G] =
    new CatsNaturalTransformation[F, G] with cats.arrow.FunctionK[F, G] {
      override protected implicit val scalazNaturalTransformation: scalaz.NaturalTransformation[F, G] = inner
    }

}

object NaturalTransformationConverter extends NaturalTransformationConverter

trait OrderConverter {

  implicit def scalazToCatsOrder[F](implicit inner: scalaz.Order[F]): cats.Order[F] =
    new CatsOrder[F] with cats.Order[F] {
      override protected implicit val scalazOrder: scalaz.Order[F] = inner
    }

}

object OrderConverter extends OrderConverter

trait CatsShow[F] {
  self: cats.Show[F] =>

  protected implicit val scalazShow: scalaz.Show[F]

  override def show(f: F): String =
    scalazShow.shows(f)
}

trait ShowConverter {

  implicit def scalazToCatsShow[F](implicit inner: scalaz.Show[F]): cats.Show[F] =
    new CatsShow[F] with cats.Show[F] {
      override protected implicit val scalazShow: scalaz.Show[F] = inner
    }

}

object ShowConverter extends ShowConverter

trait CatsTraverse[F[_], F0[_]] extends CatsFunctor[F, F0] with CatsFoldable[F, F0] {
  self: cats.Traverse[F0] =>

  import harmony.toscalaz.typeclass.ApplicativeConverter._

  protected implicit val scalazTraverse: scalaz.Traverse[F]
  override protected implicit val revTrans: NaturalTransformation[F0, F] = trans.reverse
  override protected implicit val scalazFunctor: scalaz.Functor[F] = scalazTraverse
  override protected implicit val scalazFoldable: scalaz.Foldable[F] = scalazTraverse

  override def traverse[G[_], A, B](fa: F0[A])(f: (A) => G[B])(implicit a: cats.Applicative[G]): G[F0[B]] = {
    a.ap[F[B], F0[B]](a.pure(trans.apply))(scalazTraverse.traverse(trans.reverse(fa))(f))
  }
}

trait TraverseConverter {

  implicit def scalazToCatsTraverse[F[_], F0[_]](implicit inner: scalaz.Traverse[F], t: ReversableNatTrans[F, F0]): cats.Traverse[F0] =
    new CatsTraverse[F, F0] with cats.Traverse[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val scalazTraverse: scalaz.Traverse[F] = inner

      override def imap[A, B](fa: F0[A])(f: (A) => B)(fi: (B) => A): F0[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object TraverseConverter extends TraverseConverter
