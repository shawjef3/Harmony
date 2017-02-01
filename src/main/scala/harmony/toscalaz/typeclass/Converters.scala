package harmony.toscalaz.typeclass

import harmony._
import scalaz.NaturalTransformation

trait ScalazApplicative[F[_], G[_], G0[_], F0[_]] {
  self: scalaz.Applicative[G] =>

  protected implicit val trans: ReversableNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableNatTrans[G, F, F0, G0] = ReversableNatTrans.reverse(trans)
  protected implicit val catsApplicative: cats.Applicative[F]

  override def point[A](a: => A): G[A] =
    trans(catsApplicative.pure[A](a))

  override def ap[A, B](fa: => G[A])(f: => G[(A) => B]): G[B] =
    trans(catsApplicative.ap[A, B](transReverse(f))(transReverse(fa)))

}

trait ApplicativeConverter {

  implicit def catsToScalazApplicative[F[_], G[_], G0[_], F0[_]](implicit inner: cats.Applicative[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Applicative[G] =
    new ScalazApplicative[F, G, G0, F0] with scalaz.Applicative[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsApplicative: cats.Applicative[F] = inner
    }

  implicit def catsToScalazApplicativeValue[F[_], G[_], G0[_], F0[_]](inner: cats.Applicative[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Applicative[G] =
    catsToScalazApplicative[F, G, G0, F0](inner, t)

}

object ApplicativeConverter extends ApplicativeConverter

trait ScalazApplicativePlus[F[_], G[_], G0[_], F0[_]] extends ScalazApplicative[F, G, G0, F0] {
  self: scalaz.ApplicativePlus[G] =>

  protected implicit val catsAlternative: cats.Alternative[F]
  override protected implicit val catsApplicative: cats.Applicative[F] = catsAlternative

  override def empty[A]: G[A] =
    trans(catsAlternative.empty[A])

  override def plus[A](a: G[A], b: => G[A]): G[A] =
    trans(catsAlternative.combineK(transReverse(a), transReverse(b)))

}

trait ApplicativePlusConverter {

  implicit def catsToScalazApplicativePlus[F[_], G[_], G0[_], F0[_]](implicit inner: cats.Alternative[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.ApplicativePlus[G] =
    new ScalazApplicativePlus[F, G, G0, F0] with scalaz.ApplicativePlus[G] {
      override protected implicit val catsAlternative: cats.Alternative[F] = inner
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
    }

  implicit def catsToScalazApplicativePlusValue[F[_], G[_], G0[_], F0[_]](inner: cats.Alternative[F])(t: ReversableNatTrans[F, G, G0, F0]): scalaz.ApplicativePlus[G] =
    catsToScalazApplicativePlus[F, G, G0, F0](inner, t)

}

object ApplicativePlusConverter extends ApplicativePlusConverter

trait ScalazApply[F[_], G[_], G0[_], F0[_]] extends ScalazFunctor[F, G, G0, F0] {
  self: scalaz.Apply[G] =>

  protected implicit val catsApply: cats.Apply[F]
  override protected implicit val catsFunctor: cats.Functor[F] = catsApply

  override def ap[A, B](fa: => G[A])(f: => G[(A) => B]): G[B] =
    trans(catsApply.ap(transReverse(f))(transReverse(fa)))

}

trait ApplyConverter {

  implicit def catsToScalazApply[F[_], G[_], G0[_], F0[_]](implicit inner: cats.Apply[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Apply[G] =
    new ScalazApply[F, G, G0, F0] with scalaz.Apply[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsApply: cats.Apply[F] = inner

      override def xmap[A, B](ma: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazApplyValue[F[_], G[_], G0[_], F0[_]](inner: cats.Apply[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Apply[G] =
    catsToScalazApply[F, G, G0, F0](inner, t)

}

object ApplyConverter extends ApplyConverter

trait ScalazBifoldable[F[_, _], G[_, _], G0[_, _], F0[_, _]] {
  self: scalaz.Bifoldable[G] =>

  import harmony.tocats.typeclass.MonoidConverter._
  import cats.Eval

  protected implicit val catsBifoldable: cats.Bifoldable[F]
  protected implicit val trans: BiNaturalTransformation[G, F]

  override def bifoldMap[A, B, M](fa: G[A, B])(f: (A) => M)(g: (B) => M)(implicit F: scalaz.Monoid[M]): M =
    catsBifoldable.bifoldMap(trans(fa))(f, g)

  override def bifoldRight[A, B, C](fa: G[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C = {
    //TODO: is this a correct evaluation laziness?
    def catsF(a: A, c: Eval[C]): Eval[C] =
      Eval.now(f(a, c.value))

    def catsG(b: B, c: Eval[C]): Eval[C] =
      Eval.now(g(b, c.value))

    catsBifoldable.bifoldRight[A, B, C](trans(fa), Eval.later(z))(catsF, catsG).value
  }

}

trait BifoldableConverter {

  implicit def catsToScalazBifoldable[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit inner: cats.Bifoldable[F], t: BiNaturalTransformation[G, F]): scalaz.Bifoldable[G] =
    new ScalazBifoldable[F, G, G0, F0] with scalaz.Bifoldable[G] {
      override protected implicit val catsBifoldable: cats.Bifoldable[F] = inner
      override protected implicit val trans: BiNaturalTransformation[G, F] = t
    }

  implicit def catsToScalazBifoldableValue[F[_, _], G[_, _], G0[_, _], F0[_, _]](inner: cats.Bifoldable[F])(implicit t: BiNaturalTransformation[G, F]): scalaz.Bifoldable[G] =
    catsToScalazBifoldable[F, G, G0, F0](inner, t)

}

object BifoldableConverter extends BifoldableConverter

trait ScalazBifunctor[F[_, _], G[_, _], G0[_, _], F0[_, _]] {
  self: scalaz.Bifunctor[G] =>

  protected implicit val catsBifunctor: cats.functor.Bifunctor[F]
  protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableBiNatTrans[G, F, F0, G0] = ReversableBiNatTrans.reverse(trans)

  override def bimap[A, B, C, D](fab: G[A, B])(f: (A) => C, g: (B) => D): G[C, D] =
    trans(catsBifunctor.bimap(transReverse(fab))(f, g))

}

trait BifunctorConverter {

  implicit def catsToScalazBifunctor[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit inner: cats.functor.Bifunctor[F], t: ReversableBiNatTrans[F, G, G0, F0]): scalaz.Bifunctor[G] =
    new ScalazBifunctor[F, G, G0, F0] with scalaz.Bifunctor[G] {
      override protected implicit val catsBifunctor: cats.functor.Bifunctor[F] = inner
      override protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0] = t
    }

  implicit def catsToScalazBifunctorValue[F[_, _], G[_, _], G0[_, _], F0[_, _]](inner: cats.functor.Bifunctor[F])(implicit t: ReversableBiNatTrans[F, G, G0, F0]): scalaz.Bifunctor[G] =
    catsToScalazBifunctor[F, G, G0, F0](inner, t)

}

object BifunctorConverter extends BifunctorConverter

trait ScalazBind[F[_], G[_], G0[_], F0[_]] extends ScalazApply[F, G, G0, F0] {
  self: scalaz.Bind[G] =>

  protected implicit val catsFlatMap: cats.FlatMap[F]
  override protected implicit val catsApply: cats.Apply[F] = catsFlatMap

  override def bind[A, B](fa: G[A])(f: (A) => G[B]): G[B] =
    trans(catsFlatMap.flatMap(transReverse(fa))(s => transReverse(f(s))))

}

trait BindConverter {

  implicit def catsToScalazBind[F[_], G[_], G0[_], F0[_]](implicit inner: cats.FlatMap[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Bind[G] =
    new ScalazBindRec[F, G, G0, F0] with scalaz.BindRec[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsFlatMap: cats.FlatMap[F] = inner

      override def xmap[A, B](ma: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, t)
    }

  implicit def catsToScalazBindValue[F[_], G[_], G0[_], F0[_]](inner: cats.FlatMap[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Bind[G] =
    catsToScalazBind[F, G, G0, F0](inner, t)

}

object BindConverter extends BindConverter

trait ScalazBindRec[F[_], G[_], G0[_], F0[_]] extends ScalazBind[F, G, G0, F0] {
  self: scalaz.BindRec[G] =>

  override def tailrecM[A, B](f: (A) => G[scalaz.Disjunction[A, B]])(a: A): G[B] =
    trans(catsFlatMap.tailRecM(a)((a: A) => catsFlatMap.map(transReverse(f(a)))(_.toEither)))
}

trait BindRecConverter {

  implicit def catsToScalazBindRec[F[_], G[_], G0[_], F0[_]](implicit inner: cats.FlatMap[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Bind[G] =
    new ScalazBindRec[F, G, G0, F0] with scalaz.BindRec[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsFlatMap: cats.FlatMap[F] = inner

      override def xmap[A, B](ma: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazBindRecValue[F[_], G[_], G0[_], F0[_]](inner: cats.FlatMap[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Bind[G] =
    catsToScalazBindRec[F, G, G0, F0](inner, t)

}

object BindRecConverter extends BindRecConverter

trait ScalazCategory[F[_, _], G[_, _], G0[_, _], F0[_, _]] {
  self: scalaz.Category[G] =>

  protected implicit val catsCategory: cats.arrow.Category[F]
  protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableBiNatTrans[G, F, F0, G0] = ReversableBiNatTrans.reverse(trans)

  override def id[A]: G[A, A] =
    trans(catsCategory.id[A])

  override def compose[A, B, C](f: G[B, C], g: G[A, B]): G[A, C] =
    trans(catsCategory.compose(transReverse(f), transReverse(g)))

}

trait CategoryConverter {

  implicit def catsToscalazCategory[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit inner: cats.arrow.Category[F], t: ReversableBiNatTrans[F, G, G0, F0]): scalaz.Category[G] =
    new ScalazCategory[F, G, G0, F0] with scalaz.Category[G] {
      override protected implicit val catsCategory: cats.arrow.Category[F] = inner
      override protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0] = t
    }

  implicit def catsToscalazCategoryValue[F[_, _], G[_, _], G0[_, _], F0[_, _]](inner: cats.arrow.Category[F])(implicit t: ReversableBiNatTrans[F, G, G0, F0]): scalaz.Category[G] =
    catsToscalazCategory[F, G, G0, F0](inner, t)

}

object CategoryConverter extends CategoryConverter

trait ScalazCobind[F[_], G[_], G0[_], F0[_]] extends ScalazFunctor[F, G, G0, F0] {
  self: scalaz.Cobind[G] =>

  protected implicit val catsCoflatMap: cats.CoflatMap[F]
  override protected implicit val catsFunctor: cats.Functor[F] = catsCoflatMap

  override def cobind[A, B](fa: G[A])(f: G[A] => B): G[B] =
    trans(catsCoflatMap.coflatMap(transReverse(fa))((x: F[A]) => f(trans(x))))

}

trait CoBindConverter {

  implicit def catsToScalazCobind[F[_], G[_], G0[_], F0[_]](implicit inner: cats.CoflatMap[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Cobind[G] =
    new ScalazCobind[F, G, G0, F0] with scalaz.Cobind[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsCoflatMap: cats.CoflatMap[F] = inner

      override def xmap[A, B](fa: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap(fa, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazCobindValue[F[_], G[_], G0[_], F0[_]](inner: cats.CoflatMap[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Cobind[G] =
    catsToScalazCobind[F, G, G0, F0](inner, t)

}

object CoBindConverter extends CoBindConverter

trait ScalazComonad[F[_], G[_], G0[_], F0[_]] extends ScalazCobind[F, G, G0, F0] {
  self: scalaz.Comonad[G] =>

  protected implicit val catsComonad: cats.Comonad[F]
  override protected implicit val catsCoflatMap: cats.CoflatMap[F] = catsComonad

  override def copoint[A](p: G[A]): A =
    catsComonad.extract(transReverse(p))
}

trait ComonadConverter {

  implicit def catsToScalazComonad[F[_], G[_], G0[_], F0[_]](implicit inner: cats.Comonad[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Comonad[G] =
    new ScalazComonad[F, G, G0, F0] with scalaz.Comonad[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsComonad: cats.Comonad[F] = inner

      override def xmap[A, B](fa: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap(fa, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazComonadValue[F[_], G[_], G0[_], F0[_]](inner: cats.Comonad[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Comonad[G] =
    catsToScalazComonad[F, G, G0, F0](inner, t)

}

object ComonadConverter extends ComonadConverter

trait ScalazCompose[F[_, _], G[_, _], G0[_, _], F0[_, _]] {
  self: scalaz.Compose[G] =>

  protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableBiNatTrans[G, F, F0, G0] = ReversableBiNatTrans.reverse(trans)
  protected implicit val catsCompose: cats.arrow.Compose[F]

  override def compose[A, B, C](f: G[B, C], g: G[A, B]): G[A, C] =
    trans(catsCompose.compose(transReverse(f), transReverse(g)))

}

trait ComposeConverter {

  implicit def catsToScalazCompose[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit inner: cats.arrow.Compose[F], t: ReversableBiNatTrans[F, G, G0, F0]): scalaz.Compose[G] =
    new ScalazCompose[F, G, G0, F0] with scalaz.Compose[G] {
      override protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0] = t
      override protected implicit val catsCompose: cats.arrow.Compose[F] = inner
    }

  implicit def catsToScalazComposeValue[F[_, _], G[_, _], G0[_, _], F0[_, _]](inner: cats.arrow.Compose[F])(implicit t: ReversableBiNatTrans[F, G, G0, F0]): scalaz.Compose[G] =
    catsToScalazCompose[F, G, G0, F0](inner, t)
}

object ComposeConverter extends ComposeConverter

trait ScalazEqual[F, G, G0, F0] {
  self: scalaz.Equal[G] =>

  protected implicit val catsEq: cats.Eq[F]
  protected implicit def g(f: F): G
  protected implicit def f(g: G): F

  override def equal(a1: G, a2: G): Boolean =
    catsEq.eqv(a1, a2)
}

trait EqConverter {

  implicit def catsToScalazEqual[F, G, G0, F0](implicit inner: cats.Eq[F], toG: F => G, toF: G => F): scalaz.Equal[G] =
    new ScalazEqual[F, G, G0, F0] with scalaz.Equal[G] {
      override protected implicit val catsEq: cats.Eq[F] = inner

      override protected implicit def g(f: F): G = toG(f)

      override protected implicit def f(g: G): F = toF(g)
    }

  implicit def catsToScalazEqualValue[F, G, G0, F0](inner: cats.Eq[F])(implicit toG: F => G, toF: G => F): scalaz.Equal[G] =
    catsToScalazEqual[F, G, G0, F0](inner, toG, toF)

}

object EqConverter extends EqConverter

trait ScalazContravariant[F[_], G[_], G0[_], F0[_]] {
  self: scalaz.Contravariant[G] =>

  protected implicit val catsContravariant: cats.functor.Contravariant[F]
  protected implicit val trans: ReversableNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableNatTrans[G, F, F0, G0] = ReversableNatTrans.reverse(trans)

  override def contramap[A, B](fa: G[A])(f: (B) => A): G[B] =
    trans(catsContravariant.contramap(transReverse(fa))(f))

}

trait ContravariantConverter {

  implicit def catsToScalazContravariant[F[_], G[_], G0[_], F0[_]](implicit inner: cats.functor.Contravariant[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Contravariant[G] =
    new ScalazContravariant[F, G, G0, F0] with scalaz.Contravariant[G] {
      override protected implicit val catsContravariant: cats.functor.Contravariant[F] = inner
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
    }

  implicit def catsToScalazContravariantValue[F[_], G[_], G0[_], F0[_]](inner: cats.functor.Contravariant[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Contravariant[G] =
    catsToScalazContravariant[F, G, G0, F0](inner, t)

}

object ContravariantConverter extends ContravariantConverter

trait ScalazFoldable[F[_], G[_], G0[_], F0[_]] {
  self: scalaz.Foldable[G] =>

  import harmony.tocats.typeclass.MonoidConverter._
  import cats.Eval

  protected implicit val revTrans: NaturalTransformation[G, F]
  protected implicit val catsFoldable: cats.Foldable[F]

  override def foldMap[A, B](fa: G[A])(f: (A) => B)(implicit F: scalaz.Monoid[B]): B =
    catsFoldable.foldMap(revTrans(fa))(f)

  override def foldRight[A, B](fa: G[A], z: => B)(f: (A, => B) => B): B = {
    def catsF(a: A, b: Eval[B]): Eval[B] =
      Eval.later(f(a, b.value))

    catsFoldable.foldRight(revTrans(fa), Eval.later(z))(catsF).value
  }

}

trait FoldableConverter {

  implicit def catsToScalazFoldable[F[_], G[_], G0[_], F0[_]](implicit inner: cats.Foldable[F], t: NaturalTransformation[G, F]): scalaz.Foldable[G] =
    new ScalazFoldable[F, G, G0, F0] with scalaz.Foldable[G] {
      override protected implicit val revTrans: NaturalTransformation[G, F] = t
      override protected implicit val catsFoldable: cats.Foldable[F] = inner
    }

  implicit def catsToScalazFoldableValue[F[_], G[_], G0[_], F0[_]](inner: cats.Foldable[F])(implicit t: NaturalTransformation[G, F]): scalaz.Foldable[G] =
    catsToScalazFoldable[F, G, G0, F0](inner, t)

}

object FoldableConverter extends FoldableConverter

trait ScalazFunctor[F[_], G[_], G0[_], F0[_]] extends ScalazInvariantFunctor[F, G, G0, F0] {
  self: scalaz.Functor[G] =>

  protected implicit val catsFunctor: cats.Functor[F]
  override protected implicit val catsInvariant: cats.functor.Invariant[F] = catsFunctor

  override def map[A, B](fa: G[A])(f: (A) => B): G[B] =
    trans(catsFunctor.map[A, B](transReverse(fa))(f))

}

trait FunctorConverter {

  implicit def catsToScalazFunctor[F[_], G[_], G0[_], F0[_]](implicit inner: cats.Functor[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Functor[G] =
    new ScalazFunctor[F, G, G0, F0] with scalaz.Functor[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsFunctor: cats.Functor[F] = inner

      override def xmap[A, B](ma: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazFunctorValue[F[_], G[_], G0[_], F0[_]](inner: cats.Functor[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Functor[G] =
    catsToScalazFunctor[F, G, G0, F0](inner, t)

}

object FunctorConverter extends FunctorConverter

trait ScalazInvariantFunctor[F[_], G[_], G0[_], F0[_]] {
  self: scalaz.InvariantFunctor[G] =>

  protected implicit val trans: ReversableNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableNatTrans[G, F, F0, G0] = ReversableNatTrans.reverse(trans)
  protected implicit val catsInvariant: cats.functor.Invariant[F]

  override def xmap[A, B](ma: G[A], f: (A) => B, g: (B) => A): G[B] =
    ScalazInvariantFunctor.xmap(ma, f, g)

}

object ScalazInvariantFunctor {
  def xmap[F[_], G[_], G0[_], F0[_], A, B](ma: G[A], f: (A) => B, g: (B) => A)(implicit inner: cats.functor.Invariant[F], t: ReversableNatTrans[F, G, G0, F0]): G[B] = {
    val transReverse = ReversableNatTrans.reverse(t)
    t(inner.imap(transReverse(ma))(f)(g))
  }
}

trait InvariantFunctorConverter {

  implicit def catsToScalazInvariantFunctorInstance[F[_], G[_], G0[_], F0[_]](implicit inner: cats.functor.Invariant[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.InvariantFunctor[G] =
    new ScalazInvariantFunctor[F, G, G0, F0] with scalaz.InvariantFunctor[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsInvariant: cats.functor.Invariant[F] = inner
    }

  implicit def catsToScalazInvariantFunctorValue[F[_], G[_], G0[_], F0[_]](inner: cats.functor.Invariant[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.InvariantFunctor[G] =
    catsToScalazInvariantFunctorInstance(inner, t)

}

object InvariantFunctorConverter extends InvariantFunctorConverter

trait ScalazMonad[F[_], G[_], G0[_], F0[_]]
  extends ScalazApplicative[F, G, G0, F0]
    with ScalazBindRec[F, G, G0, F0] {
  self: scalaz.Monad[G] with scalaz.BindRec[G] =>

  protected implicit val catsMonad: cats.Monad[F]
  override protected implicit val transReverse = ReversableNatTrans.reverse(trans)
  override protected implicit val catsApplicative: cats.Applicative[F] = catsMonad
  override protected implicit val catsFlatMap: cats.FlatMap[F] = catsMonad
}

trait MonadConverter {

  implicit def catsToScalazMonad[F[_], G[_], G0[_], F0[_]](implicit inner: cats.Monad[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Monad[G] =
    new ScalazMonad[F, G, G0, F0] with scalaz.Monad[G] with scalaz.BindRec[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsMonad: cats.Monad[F] = inner

      override def xmap[A, B](ma: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap[F, G, G0, F0, A, B](ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazMonadValue[F[_], G[_], G0[_], F0[_]](inner: cats.Monad[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Monad[G] =
    catsToScalazMonad[F, G, G0, F0](inner, t)

}

object MonadConverter extends MonadConverter

trait ScalazMonadError[F[_], G[_], G0[_], F0[_], S]
  extends ScalazMonad[F, G, G0, F0] {
  self: scalaz.MonadError[G, S] with scalaz.BindRec[G] =>

  protected implicit val catsMonadError: cats.MonadError[F, S]
  override protected implicit val catsMonad: cats.Monad[F] = catsMonadError

  override def raiseError[A](e: S): G[A] =
    trans(catsMonadError.raiseError(e))

  override def handleError[A](fa: G[A])(f: (S) => G[A]): G[A] =
    trans(catsMonadError.handleErrorWith(transReverse(fa))(s => transReverse(f(s))))

}

trait MonadErrorConverter {

  implicit def catsToScalazMonadError[F[_], G[_], G0[_], F0[_], S](implicit inner: cats.MonadError[F, S], t: ReversableNatTrans[F, G, G0, F0]): scalaz.MonadError[G, S] =
    new ScalazMonadError[F, G, G0, F0, S]
      with scalaz.MonadError[G, S]
      with scalaz.BindRec[G] {

      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsMonadError: cats.MonadError[F, S] = inner

      override def xmap[A, B](ma: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazMonadErrorValue[F[_], G[_], G0[_], F0[_], S](inner: cats.MonadError[F, S])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.MonadError[G, S] =
    catsToScalazMonadError[F, G, G0, F0, S](inner, t)

}

object MonadErrorConverter extends MonadErrorConverter

trait ScalazMonadReader[F[_], G[_], G0[_], F0[_], S]
  extends ScalazApplicative[F, G, G0, F0]
    with ScalazBindRec[F, G, G0, F0] {
  self: scalaz.MonadReader[G, S] with scalaz.BindRec[G] =>

  protected implicit val catsMonadReader: cats.MonadReader[F, S]
  override protected implicit val transReverse = ReversableNatTrans.reverse(trans)
  override protected implicit val catsApplicative: cats.Applicative[F] = catsMonadReader
  override protected implicit val catsFlatMap: cats.FlatMap[F] = catsMonadReader

  override def ask: G[S] =
    trans(catsMonadReader.ask)

  override def local[A](f: (S) => S)(fa: G[A]): G[A] =
    trans(catsMonadReader.local(f)(transReverse(fa)))

}

trait MonadReaderConverter {

  implicit def catsToScalazMonadReader[F[_], G[_], G0[_], F0[_], S](implicit inner: cats.MonadReader[F, S], t: ReversableNatTrans[F, G, G0, F0]): scalaz.MonadReader[G, S] =
    new ScalazMonadReader[F, G, G0, F0, S] with scalaz.MonadReader[G, S] with scalaz.BindRec[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsMonadReader: cats.MonadReader[F, S] = inner

      override def xmap[A, B](ma: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazMonadReaderValue[F[_], G[_], G0[_], F0[_], S](inner: cats.MonadReader[F, S])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.MonadReader[G, S] =
    catsToScalazMonadReader[F, G, G0, F0, S](inner, t)

}

object MonadReaderConverter extends MonadReaderConverter

trait ScalazMonadState[F[_], G[_], G0[_], F0[_], S]
  extends ScalazApplicative[F, G, G0, F0]
    with ScalazBindRec[F, G, G0, F0] {
  self: scalaz.MonadState[G, S] with scalaz.BindRec[G] =>

  protected implicit val catsMonadState: cats.MonadState[F, S]
  override protected implicit val transReverse = ReversableNatTrans.reverse(trans)
  override protected implicit val catsApplicative: cats.Applicative[F] = catsMonadState
  override protected implicit val catsFlatMap: cats.FlatMap[F] = catsMonadState

  override def init: G[S] =
    get

  override def get: G[S] =
    trans(catsMonadState.get)

  override def put(s: S): G[Unit] =
    trans(catsMonadState.set(s))
}

trait MonadStateConverter {

  implicit def catsToScalazMonadState[F[_], G[_], G0[_], F0[_], S](implicit inner: cats.MonadState[F, S], t: ReversableNatTrans[F, G, G0, F0]): scalaz.MonadState[G, S] =
    new ScalazMonadState[F, G, G0, F0, S] with scalaz.MonadState[G, S] with scalaz.BindRec[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsMonadState: cats.MonadState[F, S] = inner

      override def xmap[A, B](ma: G[A], f: (A) => B, g: (B) => A): G[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant, trans)
    }

  implicit def catsToScalazMonadStateValue[F[_], G[_], G0[_], F0[_], S](inner: cats.MonadState[F, S])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.MonadState[G, S] =
    catsToScalazMonadState[F, G, G0, F0, S](inner, t)

}

object MonadStateConverter extends MonadStateConverter

trait ScalazMonoid[F, G, G0, F0] {
  self: scalaz.Monoid[G] =>

  protected implicit val catsMonoid: cats.Monoid[F]
  protected implicit def trans(f: F): G
  protected implicit def reverse(f0: G): F

  override def zero: G =
    trans(catsMonoid.empty)

  override def append(f1: G, f2: => G): G =
    catsMonoid.combine(f1, f2)
}

trait MonoidConverter {

  implicit def catsToScalazMonoid[F, G, G0, F0](implicit inner: cats.Monoid[F], t: F => G, r: G => F): scalaz.Monoid[G] =
    new ScalazMonoid[F, G, G0, F0] with scalaz.Monoid[G] {
      override protected implicit val catsMonoid: cats.Monoid[F] = inner

      override protected implicit def trans(f: F): G = t(f)

      override protected implicit def reverse(f0: G): F = r(f0)
    }

  implicit def catsToScalazMonoidValue[F, G, G0, F0](inner: cats.Monoid[F])(implicit t: F => G, r: G => F): scalaz.Monoid[G] =
    catsToScalazMonoid[F, G, G0, F0](inner, t, r)

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

trait ScalazOrder[F, G] {
  self: scalaz.Order[G] =>

  import harmony.toscalaz.data.ComparisonConverter._

  protected implicit val catsOrder: cats.Order[F]
  protected implicit def toG(f: F): G
  protected implicit def toF(f: G): F

  override def order(x: G, y: G): scalaz.Ordering =
    catsOrder.comparison(x, y)

}

trait OrderConverter {

  implicit def catsToScalazOrder[F, G](implicit inner: cats.Order[F], t: F => G, r: G => F): scalaz.Order[G] =
    new ScalazOrder[F, G] with scalaz.Order[G] {
      override protected implicit val catsOrder: cats.Order[F] = inner

      override protected implicit def toG(f: F): G = t(f)

      override protected implicit def toF(f: G): F = r(f)
    }

  implicit def catsToScalazOrderValue[F, G](inner: cats.Order[F])(implicit t: F => G, r: G => F): scalaz.Order[G] =
    catsToScalazOrder[F, G](inner, t, r)

}

object OrderConverter extends OrderConverter

trait ScalazSemigroup[F, G] {
  self: scalaz.Semigroup[G] =>

  protected implicit val catsSemigroup: cats.Semigroup[F]
  protected implicit def toG(f: F): G
  protected implicit def toF(f: G): F

  override def append(f1: G, f2: => G): G =
    catsSemigroup.combine(f1, f2)

}

trait SemigroupConverter {

  implicit def catsToScalazSemigroup[F, G](implicit inner: cats.Semigroup[F], t: F => G, r: G => F): scalaz.Semigroup[G] =
    new ScalazSemigroup[F, G] with scalaz.Semigroup[G] {
      override protected implicit val catsSemigroup: cats.Semigroup[F] = inner

      override protected implicit def toG(f: F): G = t(f)

      override protected implicit def toF(f: G): F = r(f)
    }

  implicit def catsToScalazSemigroupValue[F, G](inner: cats.Semigroup[F])(implicit t: F => G, r: G => F): scalaz.Semigroup[G] =
    catsToScalazSemigroup[F, G](inner, t, r)

}

object SemigroupConverter extends SemigroupConverter

trait ScalazShow[F, G] {
  self: scalaz.Show[G] =>

  protected implicit val catsShow: cats.Show[F]
  protected implicit def toF(f: G): F
}

object ScalazShow {
  def shows[F, G, G0, F0](f: F0)(implicit inner: cats.Show[F], r: F0 => F): String =
    inner.show(f)
}

trait ShowConverter {

  implicit def catsToScalazShow[F, G](implicit inner: cats.Show[F], r: G => F): scalaz.Show[G] =
    new ScalazShow[F, G] with scalaz.Show[G] {
      override protected implicit val catsShow: cats.Show[F] = inner
      override protected implicit def toF(g: G): F = r(g)
      override def shows(f: G): String =
        catsShow.show(f)
    }

  implicit def catsToScalazShowValue[F, G](inner: cats.Show[F])(implicit r: G => F): scalaz.Show[G] =
    catsToScalazShow[F, G](inner, r)

}

object ShowConverter extends ShowConverter

trait ScalazTraverse[F[_], G[_], G0[_], F0[_]] {
  self: scalaz.Traverse[G] =>

  import harmony.tocats.typeclass.ApplicativeConverter._

  protected implicit val trans: ReversableNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableNatTrans[G, F, F0, G0] = ReversableNatTrans.reverse(trans)
  protected implicit val catsTraverse: cats.Traverse[F]

  override def traverseImpl[T[_]: scalaz.Applicative, A, B](fa: G[A])(f: (A) => T[B]): T[G[B]] = {
    val a = scalaz.Applicative[T]
    a.ap(catsTraverse.traverse(transReverse(fa))(f))(a.pure(trans.apply))
  }

}

trait TraverseConverter {

  implicit def catsToScalazTraverse[F[_], G[_], G0[_], F0[_]](implicit inner: cats.Traverse[F], t: ReversableNatTrans[F, G, G0, F0]): scalaz.Traverse[G] =
    new ScalazTraverse[F, G, G0, F0] with scalaz.Traverse[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val catsTraverse: cats.Traverse[F] = inner
    }

  implicit def catsToScalazTraverseValue[F[_], G[_], G0[_], F0[_]](inner: cats.Traverse[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): scalaz.Traverse[G] =
    catsToScalazTraverse[F, G, G0, F0](inner, t)

}

object TraverseConverter extends TraverseConverter
