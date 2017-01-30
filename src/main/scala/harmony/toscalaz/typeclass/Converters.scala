package harmony.toscalaz.typeclass

import harmony._
import scalaz.NaturalTransformation

trait ScalazApplicative[F[_], F0[_]] {
  self: scalaz.Applicative[F0] =>

  protected implicit val trans: ReversableNatTrans[F, F0]
  protected implicit val catsApplicative: cats.Applicative[F]

  override def point[A](a: => A): F0[A] =
    trans(catsApplicative.pure[A](a))

  override def ap[A, B](fa: => F0[A])(f: => F0[(A) => B]): F0[B] =
    trans(catsApplicative.ap[A, B](trans.reverse(f))(trans.reverse(fa)))

}

trait ApplicativeConverter {

  implicit def catsToScalazApplicative[F[_], F0[_]](implicit inner: cats.Applicative[F], t: ReversableNatTrans[F, F0]): scalaz.Applicative[F0] =
    new ScalazApplicative[F, F0] with scalaz.Applicative[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsApplicative: cats.Applicative[F] = inner
    }

  implicit def catsToScalazApplicativeValue[F[_], F0[_]](inner: cats.Applicative[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Applicative[F0] =
    catsToScalazApplicative[F, F0](inner, t)

}

object ApplicativeConverter extends ApplicativeConverter

trait ScalazApplicativePlus[F[_], F0[_]] extends ScalazApplicative[F, F0] {
  self: scalaz.ApplicativePlus[F0] =>

  protected implicit val catsAlternative: cats.Alternative[F]
  override protected implicit val catsApplicative: cats.Applicative[F] = catsAlternative

  override def empty[A]: F0[A] =
    trans(catsAlternative.empty[A])

  override def plus[A](a: F0[A], b: => F0[A]): F0[A] =
    trans(catsAlternative.combineK(trans.reverse(a), trans.reverse(b)))

}

trait ApplicativePlusConverter {

  implicit def catsToScalazApplicativePlus[F[_], F0[_]](implicit inner: cats.Alternative[F], t: ReversableNatTrans[F, F0]): scalaz.ApplicativePlus[F0] =
    new ScalazApplicativePlus[F, F0] with scalaz.ApplicativePlus[F0] {
      override protected implicit val catsAlternative: cats.Alternative[F] = inner
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
    }

  implicit def catsToScalazApplicativePlusValue[F[_], F0[_]](inner: cats.Alternative[F])(t: ReversableNatTrans[F, F0]): scalaz.ApplicativePlus[F0] =
    catsToScalazApplicativePlus[F, F0](inner, t)

}

object ApplicativePlusConverter extends ApplicativePlusConverter

trait ScalazApply[F[_], F0[_]] extends ScalazFunctor[F, F0] {
  self: scalaz.Apply[F0] =>

  protected implicit val catsApply: cats.Apply[F]
  override protected implicit val catsFunctor: cats.Functor[F] = catsApply

  override def ap[A, B](fa: => F0[A])(f: => F0[(A) => B]): F0[B] =
    trans(catsApply.ap(trans.reverse(f))(trans.reverse(fa)))

}

trait ApplyConverter {

  implicit def catsToScalazApply[F[_], F0[_]](implicit inner: cats.Apply[F], t: ReversableNatTrans[F, F0]): scalaz.Apply[F0] =
    new ScalazApply[F, F0] with scalaz.Apply[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsApply: cats.Apply[F] = inner

      override def xmap[A, B](ma: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazApplyValue[F[_], F0[_]](inner: cats.Apply[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Apply[F0] =
    catsToScalazApply[F, F0](inner, t)

}

object ApplyConverter extends ApplyConverter

trait ScalazBifoldable[F[_, _], F0[_, _]] {
  self: scalaz.Bifoldable[F0] =>

  import harmony.tocats.typeclass.MonoidConverter._
  import cats.Eval

  protected implicit val catsBifoldable: cats.Bifoldable[F]
  protected implicit val trans: BiNaturalTransformation[F0, F]

  override def bifoldMap[A, B, M](fa: F0[A, B])(f: (A) => M)(g: (B) => M)(implicit F: scalaz.Monoid[M]): M =
    catsBifoldable.bifoldMap(trans(fa))(f, g)

  override def bifoldRight[A, B, C](fa: F0[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C = {
    //TODO: is this a correct evaluation laziness?
    def catsF(a: A, c: Eval[C]): Eval[C] =
      Eval.now(f(a, c.value))

    def catsG(b: B, c: Eval[C]): Eval[C] =
      Eval.now(g(b, c.value))

    catsBifoldable.bifoldRight[A, B, C](trans(fa), Eval.later(z))(catsF, catsG).value
  }

}

trait BifoldableConverter {

  implicit def catsToScalazBifoldable[F[_, _], F0[_, _]](implicit inner: cats.Bifoldable[F], t: BiNaturalTransformation[F0, F]): scalaz.Bifoldable[F0] =
    new ScalazBifoldable[F, F0] with scalaz.Bifoldable[F0] {
      override protected implicit val catsBifoldable: cats.Bifoldable[F] = inner
      override protected implicit val trans: BiNaturalTransformation[F0, F] = t
    }

  implicit def catsToScalazBifoldableValue[F[_, _], F0[_, _]](inner: cats.Bifoldable[F])(implicit t: BiNaturalTransformation[F0, F]): scalaz.Bifoldable[F0] =
    catsToScalazBifoldable[F, F0](inner, t)

}

object BifoldableConverter extends BifoldableConverter

trait ScalazBifunctor[F[_, _], F0[_, _]] {
  self: scalaz.Bifunctor[F0] =>

  protected implicit val catsBifunctor: cats.functor.Bifunctor[F]
  protected implicit val trans: ReversableBiNatTrans[F, F0]

  override def bimap[A, B, C, D](fab: F0[A, B])(f: (A) => C, g: (B) => D): F0[C, D] =
    trans(catsBifunctor.bimap(trans.reverse(fab))(f, g))

}

trait BifunctorConverter {

  implicit def catsToScalazBifunctor[F[_, _], F0[_, _]](implicit inner: cats.functor.Bifunctor[F], t: ReversableBiNatTrans[F, F0]): scalaz.Bifunctor[F0] =
    new ScalazBifunctor[F, F0] with scalaz.Bifunctor[F0] {
      override protected implicit val catsBifunctor: cats.functor.Bifunctor[F] = inner
      override protected implicit val trans: ReversableBiNatTrans[F, F0] = t
    }

  implicit def catsToScalazBifunctorValue[F[_, _], F0[_, _]](inner: cats.functor.Bifunctor[F])(implicit t: ReversableBiNatTrans[F, F0]): scalaz.Bifunctor[F0] =
    catsToScalazBifunctor[F, F0](inner, t)

}

object BifunctorConverter extends BifunctorConverter

trait ScalazBind[F[_], F0[_]] extends ScalazApply[F, F0] {
  self: scalaz.Bind[F0] =>

  protected implicit val catsFlatMap: cats.FlatMap[F]
  override protected implicit val catsApply: cats.Apply[F] = catsFlatMap

  override def bind[A, B](fa: F0[A])(f: (A) => F0[B]): F0[B] =
    trans(catsFlatMap.flatMap(trans.reverse(fa))(s => trans.reverse(f(s))))

}

trait BindConverter {

  implicit def catsToScalazBind[F[_], F0[_]](implicit inner: cats.FlatMap[F], t: ReversableNatTrans[F, F0]): scalaz.Bind[F0] =
    new ScalazBindRec[F, F0] with scalaz.BindRec[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsFlatMap: cats.FlatMap[F] = inner

      override def xmap[A, B](ma: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, t)
    }

  implicit def catsToScalazBindValue[F[_], F0[_]](inner: cats.FlatMap[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Bind[F0] =
    catsToScalazBind[F, F0](inner, t)

}

object BindConverter extends BindConverter

trait ScalazBindRec[F[_], F0[_]] extends ScalazBind[F, F0] {
  self: scalaz.BindRec[F0] =>

  override def tailrecM[A, B](f: (A) => F0[scalaz.Disjunction[A, B]])(a: A): F0[B] =
    trans(catsFlatMap.tailRecM(a)((a: A) => catsFlatMap.map(trans.reverse(f(a)))(_.toEither)))
}

trait BindRecConverter {

  implicit def catsToScalazBindRec[F[_], F0[_]](implicit inner: cats.FlatMap[F], t: ReversableNatTrans[F, F0]): scalaz.Bind[F0] =
    new ScalazBindRec[F, F0] with scalaz.BindRec[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsFlatMap: cats.FlatMap[F] = inner

      override def xmap[A, B](ma: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazBindRecValue[F[_], F0[_]](inner: cats.FlatMap[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Bind[F0] =
    catsToScalazBindRec[F, F0](inner, t)

}

object BindRecConverter extends BindRecConverter

trait ScalazCategory[F[_, _], F0[_, _]] {
  self: scalaz.Category[F0] =>

  protected implicit val catsCategory: cats.arrow.Category[F]
  protected implicit val trans: ReversableBiNatTrans[F, F0]

  override def id[A]: F0[A, A] =
    trans(catsCategory.id[A])

  override def compose[A, B, C](f: F0[B, C], g: F0[A, B]): F0[A, C] =
    trans(catsCategory.compose(trans.reverse(f), trans.reverse(g)))

}

trait CategoryConverter {

  implicit def catsToscalazCategory[F[_, _], F0[_, _]](implicit inner: cats.arrow.Category[F], t: ReversableBiNatTrans[F, F0]): scalaz.Category[F0] =
    new ScalazCategory[F, F0] with scalaz.Category[F0] {
      override protected implicit val catsCategory: cats.arrow.Category[F] = inner
      override protected implicit val trans: ReversableBiNatTrans[F, F0] = t
    }

  implicit def catsToscalazCategoryValue[F[_, _], F0[_, _]](inner: cats.arrow.Category[F])(implicit t: ReversableBiNatTrans[F, F0]): scalaz.Category[F0] =
    catsToscalazCategory[F, F0](inner, t)

}

object CategoryConverter extends CategoryConverter

trait ScalazCobind[F[_], F0[_]] extends ScalazFunctor[F, F0] {
  self: scalaz.Cobind[F0] =>

  protected implicit val catsCoflatMap: cats.CoflatMap[F]
  override protected implicit val catsFunctor: cats.Functor[F] = catsCoflatMap

  override def cobind[A, B](fa: F0[A])(f: F0[A] => B): F0[B] =
    trans(catsCoflatMap.coflatMap(trans.reverse(fa))((x: F[A]) => f(trans(x))))

}

trait CoBindConverter {

  implicit def catsToScalazCobind[F[_], F0[_]](implicit inner: cats.CoflatMap[F], t: ReversableNatTrans[F, F0]): scalaz.Cobind[F0] =
    new ScalazCobind[F, F0] with scalaz.Cobind[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsCoflatMap: cats.CoflatMap[F] = inner

      override def xmap[A, B](fa: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap(fa, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazCobindValue[F[_], F0[_]](inner: cats.CoflatMap[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Cobind[F0] =
    catsToScalazCobind[F, F0](inner, t)

}

object CoBindConverter extends CoBindConverter

trait ScalazComonad[F[_], F0[_]] extends ScalazCobind[F, F0] {
  self: scalaz.Comonad[F0] =>

  protected implicit val catsComonad: cats.Comonad[F]
  override protected implicit val catsCoflatMap: cats.CoflatMap[F] = catsComonad

  override def copoint[A](p: F0[A]): A =
    catsComonad.extract(trans.reverse(p))
}

trait ComonadConverter {

  implicit def catsToScalazComonad[F[_], F0[_]](implicit inner: cats.Comonad[F], t: ReversableNatTrans[F, F0]): scalaz.Comonad[F0] =
    new ScalazComonad[F, F0] with scalaz.Comonad[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsComonad: cats.Comonad[F] = inner

      override def xmap[A, B](fa: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap(fa, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazComonadValue[F[_], F0[_]](inner: cats.Comonad[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Comonad[F0] =
    catsToScalazComonad[F, F0](inner, t)

}

object ComonadConverter extends ComonadConverter

trait ScalazCompose[F[_, _], F0[_, _]] {
  self: scalaz.Compose[F0] =>

  protected implicit val trans: ReversableBiNatTrans[F, F0]
  protected implicit val catsCompose: cats.arrow.Compose[F]

  override def compose[A, B, C](f: F0[B, C], g: F0[A, B]): F0[A, C] =
    trans(catsCompose.compose(trans.reverse(f), trans.reverse(g)))

}

trait ComposeConverter {

  implicit def catsToScalazCompose[F[_, _], F0[_, _]](implicit inner: cats.arrow.Compose[F], t: ReversableBiNatTrans[F, F0]): scalaz.Compose[F0] =
    new ScalazCompose[F, F0] with scalaz.Compose[F0] {
      override protected implicit val trans: ReversableBiNatTrans[F, F0] = t
      override protected implicit val catsCompose: cats.arrow.Compose[F] = inner
    }

  implicit def catsToScalazComposeValue[F[_, _], F0[_, _]](inner: cats.arrow.Compose[F])(implicit t: ReversableBiNatTrans[F, F0]): scalaz.Compose[F0] =
    catsToScalazCompose[F, F0](inner, t)
}

object ComposeConverter extends ComposeConverter

trait ScalazEqual[F, F0] {
  self: scalaz.Equal[F0] =>

  protected implicit val catsEq: cats.Eq[F]
  protected implicit def f0(f: F): F0
  protected implicit def f(f0: F0): F

  override def equal(a1: F0, a2: F0): Boolean =
    catsEq.eqv(a1, a2)
}

trait EqConverter {

  implicit def catsToScalazEqual[F, F0](implicit inner: cats.Eq[F], toF0: F => F0, toF: F0 => F): scalaz.Equal[F0] =
    new ScalazEqual[F, F0] with scalaz.Equal[F0] {
      override protected implicit val catsEq: cats.Eq[F] = inner

      override protected implicit def f0(f: F): F0 = toF0(f)

      override protected implicit def f(f0: F0): F = toF(f0)
    }

  implicit def catsToScalazEqualValue[F, F0](inner: cats.Eq[F])(implicit toF0: F => F0, toF: F0 => F): scalaz.Equal[F0] =
    catsToScalazEqual[F, F0](inner, toF0, toF)

}

object EqConverter extends EqConverter

trait ScalazContravariant[F[_], F0[_]] {
  self: scalaz.Contravariant[F0] =>

  protected implicit val catsContravariant: cats.functor.Contravariant[F]
  protected implicit val trans: ReversableNatTrans[F, F0]

  override def contramap[A, B](fa: F0[A])(f: (B) => A): F0[B] =
    trans(catsContravariant.contramap(trans.reverse(fa))(f))

}

trait ContravariantConverter {

  implicit def catsToScalazContravariant[F[_], F0[_]](implicit inner: cats.functor.Contravariant[F], t: ReversableNatTrans[F, F0]): scalaz.Contravariant[F0] =
    new ScalazContravariant[F, F0] with scalaz.Contravariant[F0] {
      override protected implicit val catsContravariant: cats.functor.Contravariant[F] = inner
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
    }

  implicit def catsToScalazContravariantValue[F[_], F0[_]](inner: cats.functor.Contravariant[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Contravariant[F0] =
    catsToScalazContravariant[F, F0](inner, t)

}

object ContravariantConverter extends ContravariantConverter

trait ScalazFoldable[F[_], F0[_]] {
  self: scalaz.Foldable[F0] =>

  import harmony.tocats.typeclass.MonoidConverter._
  import cats.Eval

  protected implicit val revTrans: NaturalTransformation[F0, F]
  protected implicit val catsFoldable: cats.Foldable[F]

  override def foldMap[A, B](fa: F0[A])(f: (A) => B)(implicit F: scalaz.Monoid[B]): B =
    catsFoldable.foldMap(revTrans(fa))(f)

  override def foldRight[A, B](fa: F0[A], z: => B)(f: (A, => B) => B): B = {
    def catsF(a: A, b: Eval[B]): Eval[B] =
      Eval.later(f(a, b.value))

    catsFoldable.foldRight(revTrans(fa), Eval.later(z))(catsF).value
  }

}

trait FoldableConverter {

  implicit def catsToScalazFoldable[F[_], F0[_]](implicit inner: cats.Foldable[F], t: NaturalTransformation[F0, F]): scalaz.Foldable[F0] =
    new ScalazFoldable[F, F0] with scalaz.Foldable[F0] {
      override protected implicit val revTrans: NaturalTransformation[F0, F] = t
      override protected implicit val catsFoldable: cats.Foldable[F] = inner
    }

  implicit def catsToScalazFoldableValue[F[_], F0[_]](inner: cats.Foldable[F])(implicit t: NaturalTransformation[F0, F]): scalaz.Foldable[F0] =
    catsToScalazFoldable[F, F0](inner, t)

}

object FoldableConverter extends FoldableConverter

trait ScalazFunctor[F[_], F0[_]] extends ScalazInvariantFunctor[F, F0] {
  self: scalaz.Functor[F0] =>

  protected implicit val catsFunctor: cats.Functor[F]
  override protected implicit val catsInvariant: cats.functor.Invariant[F] = catsFunctor

  override def map[A, B](fa: F0[A])(f: (A) => B): F0[B] =
    trans(catsFunctor.map[A, B](trans.reverse(fa))(f))

}

trait FunctorConverter {

  implicit def catsToScalazFunctor[F[_], F0[_]](implicit inner: cats.Functor[F], t: ReversableNatTrans[F, F0]): scalaz.Functor[F0] =
    new ScalazFunctor[F, F0] with scalaz.Functor[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsFunctor: cats.Functor[F] = inner

      override def xmap[A, B](ma: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazFunctorValue[F[_], F0[_]](inner: cats.Functor[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Functor[F0] =
    catsToScalazFunctor[F, F0](inner, t)

}

object FunctorConverter extends FunctorConverter

trait ScalazInvariantFunctor[F[_], F0[_]] {
  self: scalaz.InvariantFunctor[F0] =>

  protected implicit val trans: ReversableNatTrans[F, F0]
  protected implicit val catsInvariant: cats.functor.Invariant[F]

  override def xmap[A, B](ma: F0[A], f: (A) => B, g: (B) => A): F0[B] =
    ScalazInvariantFunctor.xmap(ma, f, g)

}

object ScalazInvariantFunctor {
  def xmap[F[_], F0[_], A, B](ma: F0[A], f: (A) => B, g: (B) => A)(implicit inner: cats.functor.Invariant[F], t: ReversableNatTrans[F, F0]): F0[B] =
    t(inner.imap(t.reverse(ma))(f)(g))
}

trait InvariantFunctorConverter {

  implicit def catsToScalazInvariantFunctorInstance[F[_], F0[_]](implicit inner: cats.functor.Invariant[F], t: ReversableNatTrans[F, F0]): scalaz.InvariantFunctor[F0] =
    new ScalazInvariantFunctor[F, F0] with scalaz.InvariantFunctor[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsInvariant: cats.functor.Invariant[F] = inner
    }

  implicit def catsToScalazInvariantFunctorValue[F[_], F0[_]](inner: cats.functor.Invariant[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.InvariantFunctor[F0] =
    catsToScalazInvariantFunctorInstance(inner, t)

}

object InvariantFunctorConverter extends InvariantFunctorConverter

trait ScalazMonad[F[_], F0[_]]
  extends ScalazApplicative[F, F0]
    with ScalazBindRec[F, F0] {
  self: scalaz.Monad[F0] with scalaz.BindRec[F0] =>

  protected implicit val catsMonad: cats.Monad[F]
  override protected implicit val catsApplicative: cats.Applicative[F] = catsMonad
  override protected implicit val catsFlatMap: cats.FlatMap[F] = catsMonad
}

trait MonadConverter {

  implicit def catsToScalazMonad[F[_], F0[_]](implicit inner: cats.Monad[F], t: ReversableNatTrans[F, F0]): scalaz.Monad[F0] =
    new ScalazMonad[F, F0] with scalaz.Monad[F0] with scalaz.BindRec[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsMonad: cats.Monad[F] = inner

      override def xmap[A, B](ma: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap[F, F0, A, B](ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazMonadValue[F[_], F0[_]](inner: cats.Monad[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Monad[F0] =
    catsToScalazMonad[F, F0](inner, t)

}

object MonadConverter extends MonadConverter

trait ScalazMonadError[F[_], F0[_], S]
  extends ScalazMonad[F, F0] {
  self: scalaz.MonadError[F0, S] with scalaz.BindRec[F0] =>

  protected implicit val catsMonadError: cats.MonadError[F, S]
  override protected implicit val catsMonad: cats.Monad[F] = catsMonadError

  override def raiseError[A](e: S): F0[A] =
    trans(catsMonadError.raiseError(e))

  override def handleError[A](fa: F0[A])(f: (S) => F0[A]): F0[A] =
    trans(catsMonadError.handleErrorWith(trans.reverse(fa))(s => trans.reverse(f(s))))

}

trait MonadErrorConverter {

  implicit def catsToScalazMonadError[F[_], F0[_], S](implicit inner: cats.MonadError[F, S], t: ReversableNatTrans[F, F0]): scalaz.MonadError[F0, S] =
    new ScalazMonadError[F, F0, S]
      with scalaz.MonadError[F0, S]
      with scalaz.BindRec[F0] {

      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsMonadError: cats.MonadError[F, S] = inner

      override def xmap[A, B](ma: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazMonadErrorValue[F[_], F0[_], S](inner: cats.MonadError[F, S])(implicit t: ReversableNatTrans[F, F0]): scalaz.MonadError[F0, S] =
    catsToScalazMonadError[F, F0, S](inner, t)

}

object MonadErrorConverter extends MonadErrorConverter

trait ScalazMonadReader[F[_], F0[_], S]
  extends ScalazApplicative[F, F0]
    with ScalazBindRec[F, F0] {
  self: scalaz.MonadReader[F0, S] with scalaz.BindRec[F0] =>

  protected implicit val catsMonadReader: cats.MonadReader[F, S]
  override protected implicit val catsApplicative: cats.Applicative[F] = catsMonadReader
  override protected implicit val catsFlatMap: cats.FlatMap[F] = catsMonadReader

  override def ask: F0[S] =
    trans(catsMonadReader.ask)

  override def local[A](f: (S) => S)(fa: F0[A]): F0[A] =
    trans(catsMonadReader.local(f)(trans.reverse(fa)))

}

trait MonadReaderConverter {

  implicit def catsToScalazMonadReader[F[_], F0[_], S](implicit inner: cats.MonadReader[F, S], t: ReversableNatTrans[F, F0]): scalaz.MonadReader[F0, S] =
    new ScalazMonadReader[F, F0, S] with scalaz.MonadReader[F0, S] with scalaz.BindRec[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsMonadReader: cats.MonadReader[F, S] = inner

      override def xmap[A, B](ma: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazMonadReaderValue[F[_], F0[_], S](inner: cats.MonadReader[F, S])(implicit t: ReversableNatTrans[F, F0]): scalaz.MonadReader[F0, S] =
    catsToScalazMonadReader[F, F0, S](inner, t)

}

object MonadReaderConverter extends MonadReaderConverter

trait ScalazMonadState[F[_], F0[_], S]
  extends ScalazApplicative[F, F0]
    with ScalazBindRec[F, F0] {
  self: scalaz.MonadState[F0, S] with scalaz.BindRec[F0] =>

  protected implicit val catsMonadState: cats.MonadState[F, S]
  override protected implicit val catsApplicative: cats.Applicative[F] = catsMonadState
  override protected implicit val catsFlatMap: cats.FlatMap[F] = catsMonadState

  override def init: F0[S] =
    get

  override def get: F0[S] =
    trans(catsMonadState.get)

  override def put(s: S): F0[Unit] =
    trans(catsMonadState.set(s))
}

trait MonadStateConverter {

  implicit def catsToScalazMonadState[F[_], F0[_], S](implicit inner: cats.MonadState[F, S], t: ReversableNatTrans[F, F0]): scalaz.MonadState[F0, S] =
    new ScalazMonadState[F, F0, S] with scalaz.MonadState[F0, S] with scalaz.BindRec[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsMonadState: cats.MonadState[F, S] = inner

      override def xmap[A, B](ma: F0[A], f: (A) => B, g: (B) => A): F0[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazMonadStateValue[F[_], F0[_], S](inner: cats.MonadState[F, S])(implicit t: ReversableNatTrans[F, F0]): scalaz.MonadState[F0, S] =
    catsToScalazMonadState[F, F0, S](inner, t)

}

object MonadStateConverter extends MonadStateConverter

trait ScalazMonoid[F, F0] {
  self: scalaz.Monoid[F0] =>

  protected implicit val catsMonoid: cats.Monoid[F]
  protected implicit def trans(f: F): F0
  protected implicit def reverse(f0: F0): F

  override def zero: F0 =
    trans(catsMonoid.empty)

  override def append(f1: F0, f2: => F0): F0 =
    catsMonoid.combine(f1, f2)
}

trait MonoidConverter {

  implicit def catsToScalazMonoid[F, F0](implicit inner: cats.Monoid[F], t: F => F0, r: F0 => F): scalaz.Monoid[F0] =
    new ScalazMonoid[F, F0] with scalaz.Monoid[F0] {
      override protected implicit val catsMonoid: cats.Monoid[F] = inner

      override protected implicit def trans(f: F): F0 = t(f)

      override protected implicit def reverse(f0: F0): F = r(f0)
    }

  implicit def catsToScalazMonoidValue[F, F0](inner: cats.Monoid[F])(implicit t: F => F0, r: F0 => F): scalaz.Monoid[F0] =
    catsToScalazMonoid[F, F0](inner, t, r)

}

object MonoidConverter extends MonoidConverter

trait ScalazFunctionK[F[_], F0[_], G[_], G0[_]] {
  self: scalaz.NaturalTransformation[F0, G0] =>

  protected implicit val fTrans: NaturalTransformation[F0, F]
  protected implicit val gTrans: NaturalTransformation[G, G0]
  protected implicit val catsFunctionK: cats.arrow.FunctionK[F, G]

  override def apply[A](fa: F0[A]): G0[A] =
    gTrans(catsFunctionK(fTrans(fa)))

}

trait FunctionKConverter {

  implicit def catsToScalazNaturalTransformation[F[_], F0[_], G[_], G0[_]](implicit inner: cats.arrow.FunctionK[F, G], f: NaturalTransformation[F0, F], g: NaturalTransformation[G, G0]): scalaz.NaturalTransformation[F0, G0] =
    new ScalazFunctionK[F, F0, G, G0] with scalaz.NaturalTransformation[F0, G0] {
      override protected implicit val fTrans: NaturalTransformation[F0, F] = f
      override protected implicit val gTrans: NaturalTransformation[G, G0] = g
      override protected implicit val catsFunctionK: cats.arrow.FunctionK[F, G] = inner
    }

  implicit def catsToScalazNaturalTransformationValue[F[_], F0[_], G[_], G0[_]](inner: cats.arrow.FunctionK[F, G])(implicit f: NaturalTransformation[F0, F], g: NaturalTransformation[G, G0]): scalaz.NaturalTransformation[F0, G0] =
    catsToScalazNaturalTransformation[F, F0, G, G0](inner, f, g)
}

object FunctionKConverter extends FunctionKConverter

trait ScalazOrder[F, F0] {
  self: scalaz.Order[F0] =>

  import harmony.toscalaz.data.ComparisonConverter._

  protected implicit val catsOrder: cats.Order[F]
  protected implicit def toF0(f: F): F0
  protected implicit def toF(f: F0): F

  override def order(x: F0, y: F0): scalaz.Ordering =
    catsOrder.comparison(x, y)

}

trait OrderConverter {

  implicit def catsToScalazOrder[F, F0](implicit inner: cats.Order[F], t: F => F0, r: F0 => F): scalaz.Order[F0] =
    new ScalazOrder[F, F0] with scalaz.Order[F0] {
      override protected implicit val catsOrder: cats.Order[F] = inner

      override protected implicit def toF0(f: F): F0 = t(f)

      override protected implicit def toF(f: F0): F = r(f)
    }

  implicit def catsToScalazOrderValue[F, F0](inner: cats.Order[F])(implicit t: F => F0, r: F0 => F): scalaz.Order[F0] =
    catsToScalazOrder[F, F0](inner, t, r)

}

object OrderConverter extends OrderConverter

trait ScalazSemigroup[F, F0] {
  self: scalaz.Semigroup[F0] =>

  protected implicit val catsSemigroup: cats.Semigroup[F]
  protected implicit def toF0(f: F): F0
  protected implicit def toF(f: F0): F

  override def append(f1: F0, f2: => F0): F0 =
    catsSemigroup.combine(f1, f2)

}

trait SemigroupConverter {

  implicit def catsToScalazSemigroup[F, F0](implicit inner: cats.Semigroup[F], t: F => F0, r: F0 => F): scalaz.Semigroup[F0] =
    new ScalazSemigroup[F, F0] with scalaz.Semigroup[F0] {
      override protected implicit val catsSemigroup: cats.Semigroup[F] = inner

      override protected implicit def toF0(f: F): F0 = t(f)

      override protected implicit def toF(f: F0): F = r(f)
    }

  implicit def catsToScalazSemigroupValue[F, F0](inner: cats.Semigroup[F])(implicit t: F => F0, r: F0 => F): scalaz.Semigroup[F0] =
    catsToScalazSemigroup[F, F0](inner, t, r)

}

object SemigroupConverter extends SemigroupConverter

trait ScalazShow[F, F0] {
  self: scalaz.Show[F0] =>

  protected implicit val catsShow: cats.Show[F]
  protected implicit def toF(f: F0): F
}

object ScalazShow {
  def shows[F, F0](f: F0)(implicit inner: cats.Show[F], r: F0 => F): String =
    inner.show(f)
}

trait ShowConverter {

  implicit def catsToScalazShow[F, F0](implicit inner: cats.Show[F], r: F0 => F): scalaz.Show[F0] =
    new ScalazShow[F, F0] with scalaz.Show[F0] {
      override protected implicit val catsShow: cats.Show[F] = inner
            override protected implicit def toF(f: F0): F = r(f)
      override def shows(f: F0): String =
        catsShow.show(f)
    }

  implicit def catsToScalazShowValue[F, F0](inner: cats.Show[F])(implicit r: F0 => F): scalaz.Show[F0] =
    catsToScalazShow[F, F0](inner, r)

}

object ShowConverter extends ShowConverter

trait ScalazTraverse[F[_], F0[_]] {
  self: scalaz.Traverse[F0] =>

  import harmony.tocats.typeclass.ApplicativeConverter._

  protected implicit val trans: ReversableNatTrans[F, F0]
  protected implicit val catsTraverse: cats.Traverse[F]

  override def traverseImpl[G[_]: scalaz.Applicative, A, B](fa: F0[A])(f: (A) => G[B]): G[F0[B]] = {
    val a = implicitly[scalaz.Applicative[G]]
    a.ap(catsTraverse.traverse(trans.reverse(fa))(f))(a.pure(trans.apply))
  }

}

trait TraverseConverter {

  implicit def catsToScalazTraverse[F[_], F0[_]](implicit inner: cats.Traverse[F], t: ReversableNatTrans[F, F0]): scalaz.Traverse[F0] =
    new ScalazTraverse[F, F0] with scalaz.Traverse[F0] {
      override protected implicit val trans: ReversableNatTrans[F, F0] = t
      override protected implicit val catsTraverse: cats.Traverse[F] = inner
    }

  implicit def catsToScalazTraverseValue[F[_], F0[_]](inner: cats.Traverse[F])(implicit t: ReversableNatTrans[F, F0]): scalaz.Traverse[F0] =
    catsToScalazTraverse[F, F0](inner, t)

}

object TraverseConverter extends TraverseConverter
