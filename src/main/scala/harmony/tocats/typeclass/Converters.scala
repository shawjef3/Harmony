package harmony.tocats.typeclass

import harmony._

trait CatsAlternative[F[_], G[_], G0[_], F0[_]] extends CatsApplicative[F, G, G0, F0] {
  self: cats.Alternative[G] =>

  protected implicit val scalazApplicativePlus: scalaz.ApplicativePlus[F]
  override protected implicit val scalazApplicative: scalaz.Applicative[F] = scalazApplicativePlus

  override def empty[A]: G[A] =
    trans(scalazApplicativePlus.empty[A])

  override def combineK[A](x: G[A], y: G[A]): G[A] =
    trans(scalazApplicativePlus.plus(transReverse(x), transReverse(y)))

}

trait AlternativeConverter {

  implicit def scalazToCatsAlternative[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.ApplicativePlus[F], t: ReversableNatTrans[F, G, G0, F0]): cats.Alternative[G] =
    new CatsAlternative[F, G, G0, F0] with cats.Alternative[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazApplicativePlus: scalaz.ApplicativePlus[F] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, t)
    }

}

object AlternativeConverter extends AlternativeConverter

trait CatsApplicative[F[_], G[_], G0[_], F0[_]] extends CatsApply[F, G, G0, F0] {
  self: cats.Applicative[G] =>

  protected implicit val scalazApplicative: scalaz.Applicative[F]
  override protected implicit val scalazApply: scalaz.Apply[F] = scalazApplicative

  override def ap[A, B](ff: G[(A) => B])(fa: G[A]): G[B] =
    trans(scalazApplicative.ap(transReverse(fa))(transReverse(ff)))

  override def pure[A](x: A): G[A] =
    trans(scalazApplicative.point(x))

}

trait ApplicativeConverter {

  implicit def scalazToCatsApplicative[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.Applicative[F], t: ReversableNatTrans[F, G, G0, F0]): cats.Applicative[G] =
    new CatsApplicative[F, G, G0, F0] with cats.Applicative[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazApplicative: scalaz.Applicative[F] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object ApplicativeConverter extends ApplicativeConverter

trait CatsArrow[F[_, _], G[_, _], G0[_, _], F0[_, _]] {
  self: cats.arrow.Arrow[G] =>

  protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableBiNatTrans[G, F, F0, G0] = ReversableBiNatTrans.reverse(trans)
  protected implicit val scalazArrow: scalaz.Arrow[F]

  override def lift[A, B](f: (A) => B): G[A, B] =
    trans(scalazArrow.arr(f))

  override def first[A, B, C](fa: G[A, B]): G[(A, C), (B, C)] =
    trans(scalazArrow.first(transReverse(fa)))

  override def id[A]: G[A, A] =
    trans(scalazArrow.id[A])

  override def compose[A, B, C](f: G[B, C], g: G[A, B]): G[A, C] =
    trans(scalazArrow.compose(transReverse(f), transReverse(g)))
}

trait ArrowConverter {

  implicit def scalazToCatsArrow[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit inner: scalaz.Arrow[F], t: ReversableBiNatTrans[F, G, G0, F0]): cats.arrow.Arrow[G] =
    new CatsArrow[F, G, G0, F0] with cats.arrow.Arrow[G] {
      override protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazArrow: scalaz.Arrow[F] = inner
    }

}

object ArrowConverter extends ArrowConverter

trait CatsApply[F[_], G[_], G0[_], F0[_]] extends CatsFunctor[F, G, G0, F0] {
  self: cats.Apply[G] =>

  protected implicit val scalazApply: scalaz.Apply[F]
  override protected implicit val scalazFunctor: scalaz.Functor[F] = scalazApply

  override def ap[A, B](ff: G[(A) => B])(fa: G[A]): G[B] =
    trans(scalazApply.ap(transReverse(fa))(transReverse(ff)))

}

trait ApplyConverter {

  implicit def scalazToCatsApply[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.Apply[F], t: ReversableNatTrans[F, G, G0, F0]): cats.Apply[G] =
    new CatsApply[F, G, G0, F0] with cats.Apply[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazApply: scalaz.Apply[F] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object ApplyConverter extends ApplyConverter

trait CatsBifoldable[F[_, _], G[_, _]] {
  self: cats.Bifoldable[G] =>

  import cats.Eval

  protected implicit val scalazBifoldable: scalaz.Bifoldable[F]
  protected implicit val trans: BiNaturalTransformation[G, F]

  override def bifoldLeft[A, B, C](fab: G[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    scalazBifoldable.bifoldLeft(trans(fab), c)(f)(g)

  override def bifoldRight[A, B, C](
    fab: G[A, B],
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

  implicit def scalazToCatsBifoldable[F[_, _], G[_, _]](implicit inner: scalaz.Bifoldable[F], t: BiNaturalTransformation[G, F]): cats.Bifoldable[G] =
    new CatsBifoldable[F, G] with cats.Bifoldable[G] {
      override protected implicit val scalazBifoldable: scalaz.Bifoldable[F] = inner
      override protected implicit val trans: BiNaturalTransformation[G, F] = t
    }

}

object BifoldableConverter extends BifoldableConverter

trait CatsBifunctor[F[_, _], G[_, _], G0[_, _], F0[_, _]] {
  self: cats.functor.Bifunctor[G] =>

  protected implicit val scalazBifunctor: scalaz.Bifunctor[F]
  protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableBiNatTrans[G, F, F0, G0] = ReversableBiNatTrans.reverse(trans)

  override def bimap[A, B, C, D](fab: G[A, B])(f: (A) => C, g: (B) => D): G[C, D] =
    trans(scalazBifunctor.bimap(transReverse(fab))(f, g))

}

trait BifunctorConverter {

  implicit def scalazToCatsBifunctor[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit inner: scalaz.Bifunctor[F], t: ReversableBiNatTrans[F, G, G0, F0]): cats.functor.Bifunctor[G] =
    new CatsBifunctor[F, G, G0, F0] with cats.functor.Bifunctor[G] {
      override protected implicit val scalazBifunctor: scalaz.Bifunctor[F] = inner
      override protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0] = t
    }

}

object BifunctorConverter extends BifunctorConverter

trait CatsBind[F[_], G[_], G0[_], F0[_]] extends CatsApply[F, G, G0, F0] {
  self: cats.FlatMap[G] =>

  protected implicit val scalazBindRec: scalaz.BindRec[F]
  override protected implicit val scalazApply: scalaz.Apply[F] = scalazBindRec

  override def flatMap[A, B](fa: G[A])(f: (A) => G[B]): G[B] =
    trans(scalazBindRec.bind(transReverse(fa))(a => transReverse(f(a))))

  override def tailRecM[A, B](a: A)(f: (A) => G[Either[A, B]]): G[B] =
    trans(scalazBindRec.tailrecM((a: A) => scalazBindRec.map(transReverse(f(a)))(scalaz.Disjunction.fromEither))(a))

}

trait BindConverter {

  implicit def scalazToCatsFlatMap[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.BindRec[F], t: ReversableNatTrans[F, G, G0, F0]): cats.FlatMap[G] =
    new CatsBind[F, G, G0, F0] with cats.FlatMap[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazBindRec: scalaz.BindRec[F] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object BindConverter extends BindConverter

trait CatsCategory[F[_, _], G[_, _], G0[_, _], F0[_, _]] {
  self: cats.arrow.Category[G] =>

  protected implicit val scalazCategory: scalaz.Category[F]
  protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableBiNatTrans[G, F, F0, G0] = ReversableBiNatTrans.reverse(trans)

  override def id[A]: G[A, A] =
    trans(scalazCategory.id[A])

  override def compose[A, B, C](f: G[B, C], g: G[A, B]): G[A, C] =
    trans(scalazCategory.compose(transReverse(f), transReverse(g)))

}

trait CategoryConverter {

  implicit def scalazToCatsCategory[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit inner: scalaz.Category[F], t: ReversableBiNatTrans[F, G, G0, F0]): cats.arrow.Category[G] =
    new CatsCategory[F, G, G0, F0] with cats.arrow.Category[G] {
      override protected implicit val scalazCategory: scalaz.Category[F] = inner
      override protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0] = t
    }

  implicit def scalazToCatsCategoryValue[F[_, _], G[_, _], G0[_, _], F0[_, _]](inner: scalaz.Category[F])(implicit t: ReversableBiNatTrans[F, G, G0, F0]): cats.arrow.Category[G] =
    scalazToCatsCategory[F, G, G0, F0](inner, t)

}

object CategoryConverter extends CategoryConverter

trait CatsChoice[F[_, _], G[_, _], G0[_, _], F0[_, _]] {
  self: cats.arrow.Choice[G] =>

  protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableBiNatTrans[G, F, F0, G0] = ReversableBiNatTrans.reverse(trans)
  protected implicit val scalazChoice: scalaz.Choice[F]

  override def choice[A, B, C](f: G[A, C], g: G[B, C]): G[Either[A, B], C] =
    ???

  override def id[A]: G[A, A] =
    trans(scalazChoice.id[A])

  override def compose[A, B, C](f: G[B, C], g: G[A, B]): G[A, C] =
    trans(scalazChoice.compose(transReverse(f), transReverse(g)))
}

trait ChoiceConverter {

  implicit def scalazToCatsChoice[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit inner: scalaz.Choice[F], t: ReversableBiNatTrans[F, G, G0, F0]): cats.arrow.Choice[G] =
    new CatsChoice[F, G, G0, F0] with cats.arrow.Choice[G] {
      override protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazChoice: scalaz.Choice[F] = inner
    }

}

object ChoiceConverter extends ChoiceConverter

trait CatsCoflatMap[F[_], G[_], G0[_], F0[_]] extends CatsFunctor[F, G, G0, F0] {
  self: cats.CoflatMap[G] =>

  protected implicit val scalazCobind: scalaz.Cobind[F]
  override protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F] = scalazCobind
  override protected implicit val scalazFunctor: scalaz.Functor[F] = scalazCobind

  override def coflatMap[A, B](fa: G[A])(f: (G[A]) => B): G[B] =
    trans(scalazCobind.cobind(transReverse(fa))((a: F[A]) => f(trans(a))))

}

trait CoBindConverter {

  implicit def scalazToCatsCoFlatMap[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.Cobind[F], t: ReversableNatTrans[F, G, G0, F0]): cats.CoflatMap[G] =
    new CatsCoflatMap[F, G, G0, F0] with cats.CoflatMap[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazCobind: scalaz.Cobind[F] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object CoBindConverter extends CoBindConverter

trait CatsComonad[F[_], G[_], G0[_], F0[_]] extends CatsCoflatMap[F, G, G0, F0] {
  self: cats.Comonad[G] =>

  protected implicit val scalazComonad: scalaz.Comonad[F]
  override protected implicit val scalazCobind: scalaz.Cobind[F] = scalazComonad

  override def extract[A](x: G[A]): A =
    scalazComonad.copoint(transReverse(x))

}

trait ComonadConverter {

  implicit def scalazToCatsComonad[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.Comonad[F], t: ReversableNatTrans[F, G, G0, F0]): cats.Comonad[G] =
    new CatsComonad[F, G, G0, F0] with cats.Comonad[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazComonad: scalaz.Comonad[F] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object ComonadConverter extends ComonadConverter

trait CatsCompose[F[_, _], G[_, _], G0[_, _], F0[_, _]] {
  self: cats.arrow.Compose[G] =>

  protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableBiNatTrans[G, F, F0, G0] = ReversableBiNatTrans.reverse(trans)
  protected implicit val scalazCompose: scalaz.Compose[F]

  override def compose[A, B, C](f: G[B, C], g: G[A, B]): G[A, C] =
    trans(scalazCompose.compose(transReverse(f), transReverse(g)))

}

trait ComposeConverter {

  implicit def scalazToCatsCompose[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit inner: scalaz.Compose[F], t: ReversableBiNatTrans[F, G, G0, F0]): cats.arrow.Compose[G] =
    new CatsCompose[F, G, G0, F0] with cats.arrow.Compose[G] {
      override protected implicit val trans: ReversableBiNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazCompose: scalaz.Compose[F] = inner
    }

}

object ComposeConverter extends ComposeConverter

trait CatsContravariant[F[_], G[_], G0[_], F0[_]] {
  self: cats.functor.Contravariant[G] =>

  protected implicit val scalazContravariant: scalaz.Contravariant[F]
  protected implicit val trans: ReversableNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableNatTrans[G, F, F0, G0] = ReversableNatTrans.reverse(trans)

  override def contramap[A, B](fa: G[A])(f: (B) => A): G[B] =
    trans(scalazContravariant.contramap(transReverse(fa))(f))

}

trait ContravariantConverter {

  implicit def scalazToCatsContravariant[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.Contravariant[F], t: ReversableNatTrans[F, G, G0, F0]): cats.functor.Contravariant[G] =
    new CatsContravariant[F, G, G0, F0] with cats.functor.Contravariant[G] {
      override protected implicit val scalazContravariant: scalaz.Contravariant[F] = inner
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
    }

}

object ContravariantConverter extends ContravariantConverter

trait CatsEq[F, G, G0, F0] {
  self: cats.Eq[F0] =>

  protected implicit val scalazEqual: scalaz.Equal[F]
  protected implicit def f0(f: F): F0
  protected implicit def f(f0: F0): F

  override def eqv(x: F0, y: F0): Boolean =
    scalazEqual.equal(x, y)
}

trait EqualConverter {

  implicit def catsToScalazEqual[F, G, G0, F0](implicit inner: scalaz.Equal[F], toF0: F => F0, toF: F0 => F): cats.Eq[F0] =
    new CatsEq[F, G, G0, F0] with cats.Eq[F0] {
      override protected implicit val scalazEqual: scalaz.Equal[F] = inner

      override protected implicit def f0(f: F): F0 = toF0(f)

      override protected implicit def f(f0: F0): F = toF(f0)
    }

}

object EqualConverter extends EqualConverter

trait CatsFoldable[F[_], G[_], G0[_], F0[_]] {
  self: cats.Foldable[G] =>

  import cats.Eval

  protected implicit val transReverse: NaturalTransformation[G, F]
  protected implicit val scalazFoldable: scalaz.Foldable[F]

  override def foldLeft[A, B](fa: G[A], b: B)(f: (B, A) => B): B =
    scalazFoldable.foldLeft(transReverse(fa), b)(f)

  override def foldRight[A, B](fa: G[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    def catsF(a: A, b: => Eval[B]): Eval[B] =
      f(a, b)

    scalazFoldable.foldRight[A, Eval[B]](transReverse(fa), lb)(catsF)
  }
}

trait FoldableConverter {

  implicit def scalazToCatsFoldable[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.Foldable[F], t: NaturalTransformation[G, F]): cats.Foldable[G] =
    new CatsFoldable[F, G, G0, F0] with cats.Foldable[G] {
      override protected implicit val transReverse: NaturalTransformation[G, F] = t
      override protected implicit val scalazFoldable: scalaz.Foldable[F] = inner
    }

//  implicit def scalazToCatsFoldableId[F[_]](implicit inner: scalaz.Foldable[F]): cats.Foldable[F] = {
//    implicit val t = NaturalTransformation.refl[F]
//    scalazToCatsFoldable[F, F, F, F]
//  }

}

object FoldableConverter extends FoldableConverter

trait CatsFunctor[F[_], G[_], G0[_], F0[_]] extends CatsInvariantFunctor[F, G, G0, F0] {
  self: cats.Functor[G] =>

  protected implicit val scalazFunctor: scalaz.Functor[F]
  override protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F] = scalazFunctor

  override def map[A, B](fa: G[A])(f: (A) => B): G[B] =
    trans(scalazFunctor.map(transReverse(fa))(f))

}

trait FunctorConverter {

  implicit def scalazToCatsFunctor[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.Functor[F], t: ReversableNatTrans[F, G, G0, F0]): cats.Functor[G] =
    new CatsFunctor[F, G, G0, F0] with cats.Functor[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazFunctor: scalaz.Functor[F] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

//  implicit def scalazToCatsFunctorId[F[_]](implicit inner: scalaz.Functor[F]): cats.Functor[F] =
//    scalazToCatsFunctor[F, F]

  implicit def scalazToCatsFunctorValue[F[_], G[_], G0[_], F0[_]](inner: scalaz.Functor[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): cats.Functor[G] =
    scalazToCatsFunctor[F, G, G0, F0](inner, t)

}

object FunctorConverter extends FunctorConverter

trait CatsInvariantFunctor[F[_], G[_], G0[_], F0[_]] {
  self: cats.functor.Invariant[G] =>

  protected implicit val trans: ReversableNatTrans[F, G, G0, F0]
  protected implicit val transReverse: ReversableNatTrans[G, F, F0, G0] = ReversableNatTrans.reverse(trans)
  protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F]

  override def imap[A, B](fa: G[A])(f: (A) => B)(g: (B) => A): G[B] =
    CatsInvariantFunctor.imap(fa)(f, g)
}

object CatsInvariantFunctor {
  def imap[F[_], G[_], G0[_], F0[_], A, B](fa: G[A])(f: (A) => B, g: (B) => A)(implicit inner: scalaz.InvariantFunctor[F], t: ReversableNatTrans[F, G, G0, F0]): G[B] =
    t(inner.xmap(ReversableNatTrans.reverse(t)(fa), f, g))
}

trait InvariantFunctorConverter {

  implicit def scalazToCatsInvariantInstance[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.InvariantFunctor[F], t: ReversableNatTrans[F, G, G0, F0]): cats.functor.Invariant[G] =
    new CatsInvariantFunctor[F, G, G0, F0] with cats.functor.Invariant[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazInvariantFunctor: scalaz.InvariantFunctor[F] = inner
    }

  implicit def scalazToCatsInvariantValue[F[_], G[_], G0[_], F0[_]](inner: scalaz.InvariantFunctor[F])(implicit t: ReversableNatTrans[F, G, G0, F0]): cats.functor.Invariant[G] =
    scalazToCatsInvariantInstance[F, G, G0, F0](inner, t)

}

object InvariantFunctorConverter extends InvariantFunctorConverter

trait CatsMonad[F[_], G[_], G0[_], F0[_]]
  extends CatsBind[F, G, G0, F0]
    with CatsApplicative[F, G, G0, F0] {
  self: cats.Monad[G] =>

  protected implicit val scalazMonad: scalaz.Monad[F]
  override protected implicit val scalazApplicative: scalaz.Applicative[F] = scalazMonad
}

trait MonadConverter {

  implicit def scalazToCatsMonad[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.Monad[F], inner0: scalaz.BindRec[F], t: ReversableNatTrans[F, G, G0, F0]): cats.Monad[G] =
    new CatsMonad[F, G, G0, F0] with cats.Monad[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazBindRec = inner0
      override protected implicit val scalazMonad: scalaz.Monad[F] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object MonadConverter extends MonadConverter

trait CatsMonadError[F[_], G[_], G0[_], F0[_], E] extends CatsMonad[F, G, G0, F0] {
  self: cats.MonadError[G, E] =>

  protected implicit val scalazMonadError: scalaz.MonadError[F, E]
  override protected implicit val scalazMonad: scalaz.Monad[F] = scalazMonadError

  override def raiseError[A](e: E): G[A] =
    trans(scalazMonadError.raiseError(e))

  override def handleErrorWith[A](fa: G[A])(f: (E) => G[A]): G[A] =
    trans(scalazMonadError.handleError(transReverse(fa))(e => transReverse(f(e))))

  override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
    CatsInvariantFunctor.imap(fa)(f, fi)

}

trait MonadErrorConverter {

  implicit def scalazToCatsMonadError[F[_], G[_], G0[_], F0[_], E](implicit inner: scalaz.MonadError[F, E], inner0: scalaz.BindRec[F], t: ReversableNatTrans[F, G, G0, F0]): cats.MonadError[G, E] =
    new CatsMonadError[F, G, G0, F0, E] with cats.MonadError[G, E] {

      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazBindRec = inner0
      override protected implicit val scalazMonadError: scalaz.MonadError[F, E] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object MonadErrorConverter extends MonadErrorConverter

trait CatsMonadReader[F[_], G[_], G0[_], F0[_], S] extends CatsMonad[F, G, G0, F0] {
  self: cats.MonadReader[G, S] =>

  protected implicit val scalazMonadReader: scalaz.MonadReader[F, S]
  override protected implicit val scalazMonad: scalaz.Monad[F] = scalazMonadReader

  override def ask: G[S] =
    trans(scalazMonadReader.ask)

  override def local[A](f: (S) => S)(fa: G[A]): G[A] =
    trans(scalazMonadReader.local(f)(transReverse(fa)))

}

trait MonadReaderConverter {

  implicit def scalazToCatsMonadReader[F[_], G[_], G0[_], F0[_], S](implicit inner: scalaz.MonadReader[F, S], inner0: scalaz.BindRec[F], t: ReversableNatTrans[F, G, G0, F0]): cats.MonadReader[G, S] =
    new CatsMonadReader[F, G, G0, F0, S] with cats.MonadReader[G, S] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazMonadReader: scalaz.MonadReader[F, S] = inner
      override protected implicit val scalazBindRec: scalaz.BindRec[F] = inner0

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(inner, trans)
    }

}

object MonadReaderConverter extends MonadReaderConverter

trait CatsMonadState[F[_], G[_], G0[_], F0[_], S]
  extends CatsMonad[F, G, G0, F0] {
  self: cats.MonadState[G, S] =>

  protected implicit val scalazMonadState: scalaz.MonadState[F, S]
  override protected implicit val scalazMonad: scalaz.Monad[F] = scalazMonadState

  override def get: G[S] =
    trans(scalazMonadState.get)

  override def set(s: S): G[Unit] =
    trans(scalazMonadState.put(s))

}

trait MonadStateConverter {

  implicit def scalazToCatsMonadState[F[_], G[_], G0[_], F0[_], S](implicit inner: scalaz.MonadState[F, S], inner0: scalaz.BindRec[F], t: ReversableNatTrans[F, G, G0, F0]): cats.Monad[G] =
    new CatsMonadState[F, G, G0, F0, S] with cats.MonadState[G, S] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazMonadState: scalaz.MonadState[F, S] = inner
      override protected implicit val scalazBindRec: scalaz.BindRec[F] = inner0

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(inner, trans)
    }

}

object MonadStateConverter extends MonadStateConverter

trait CatsMonoid[F, G, G0, F0] {
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

  implicit def scalazToCatsMonoid[F, G, G0, F0](implicit inner: scalaz.Monoid[F], t: F => F0, r: F0 => F): cats.Monoid[F0] =
    new CatsMonoid[F, G, G0, F0] with cats.Monoid[F0] {
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

trait CatsTraverse[F[_], G[_], G0[_], F0[_]] extends CatsFunctor[F, G, G0, F0] with CatsFoldable[F, G, G0, F0] {
  self: cats.Traverse[G] =>

  import harmony.toscalaz.typeclass.ApplicativeConverter._

  protected implicit val scalazTraverse: scalaz.Traverse[F]
  override protected implicit val scalazFunctor: scalaz.Functor[F] = scalazTraverse
  override protected implicit val scalazFoldable: scalaz.Foldable[F] = scalazTraverse

  override def traverse[T[_], A, B](fa: G[A])(f: (A) => T[B])(implicit a: cats.Applicative[T]): T[G[B]] = {
    a.ap[F[B], G[B]](a.pure(trans.apply))(scalazTraverse.traverse(transReverse(fa))(f))
  }
}

trait TraverseConverter {

  implicit def scalazToCatsTraverse[F[_], G[_], G0[_], F0[_]](implicit inner: scalaz.Traverse[F], t: ReversableNatTrans[F, G, G0, F0]): cats.Traverse[G] =
    new CatsTraverse[F, G, G0, F0] with cats.Traverse[G] {
      override protected implicit val trans: ReversableNatTrans[F, G, G0, F0] = t
      override protected implicit val scalazTraverse: scalaz.Traverse[F] = inner

      override def imap[A, B](fa: G[A])(f: (A) => B)(fi: (B) => A): G[B] =
        CatsInvariantFunctor.imap(fa)(f, fi)(scalazInvariantFunctor, trans)
    }

}

object TraverseConverter extends TraverseConverter
