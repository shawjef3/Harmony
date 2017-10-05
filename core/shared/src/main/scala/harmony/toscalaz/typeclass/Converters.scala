package harmony.toscalaz.typeclass

import cats.Apply

trait ScalazApplicative[F[_]] {
  self: scalaz.Applicative[F] =>

  protected implicit def catsApplicative: cats.Applicative[F]

  override def point[A](a: => A): F[A] =
    catsApplicative.pure[A](a)

  override def ap[A, B](fa: => F[A])(f: => F[(A) => B]): F[B] =
    catsApplicative.ap[A, B](f)(fa)

}

trait ApplicativeConverter {

  implicit def catsToScalazApplicative[F[_]](implicit inner: cats.Applicative[F]): scalaz.Applicative[F] =
    new ScalazApplicative[F] with scalaz.Applicative[F] {
      override protected implicit val catsApplicative: cats.Applicative[F] = inner
    }

  implicit def catsToScalazApplicativeValue[F[_]](inner: cats.Applicative[F]): scalaz.Applicative[F] =
    catsToScalazApplicative[F](inner)

}

object ApplicativeConverter extends ApplicativeConverter

trait ScalazApplicativePlus[F[_]] extends ScalazApplicative[F] {
  self: scalaz.ApplicativePlus[F] =>

  protected implicit def catsAlternative: cats.Alternative[F]
  override protected implicit lazy val catsApplicative: cats.Applicative[F] = catsAlternative

  override def empty[A]: F[A] =
    catsAlternative.empty[A]

  override def plus[A](a: F[A], b: => F[A]): F[A] =
    catsAlternative.combineK(a, b)

}

trait ApplicativePlusConverter {

  implicit def catsToScalazApplicativePlus[F[_]](implicit inner: cats.Alternative[F]): scalaz.ApplicativePlus[F] =
    new ScalazApplicativePlus[F] with scalaz.ApplicativePlus[F] {
      override protected implicit val catsAlternative: cats.Alternative[F] = inner
    }

  implicit def catsToScalazApplicativePlusValue[F[_]](inner: cats.Alternative[F]): scalaz.ApplicativePlus[F] =
    catsToScalazApplicativePlus[F](inner)

}

object ApplicativePlusConverter extends ApplicativePlusConverter

trait ScalazApply[F[_]] extends ScalazFunctor[F] {
  self: scalaz.Apply[F] =>

  protected implicit def catsApply: cats.Apply[F]
  override protected implicit lazy val catsFunctor: cats.Functor[F] = catsApply

  override def ap[A, B](fa: => F[A])(f: => F[(A) => B]): F[B] =
    catsApply.ap(f)(fa)

}

trait ApplyConverter {

  implicit def catsToScalazApply[F[_]](implicit inner: cats.Apply[F]): scalaz.Apply[F] =
    new ScalazApply[F] with scalaz.Apply[F] {
      override protected implicit val catsApply: cats.Apply[F] = inner

      override def xmap[A, B](ma: F[A], f: (A) => B, g: (B) => A): F[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant)
    }

  implicit def catsToScalazApplyValue[F[_]](inner: cats.Apply[F]): scalaz.Apply[F] =
    catsToScalazApply[F](inner)

}

object ApplyConverter extends ApplyConverter

trait ScalazBifoldable[F[_, _]] {
  self: scalaz.Bifoldable[F] =>

  import harmony.tocats.typeclass.MonoidConverter._
  import cats.Eval

  protected implicit def catsBifoldable: cats.Bifoldable[F]

  override def bifoldMap[A, B, M](fa: F[A, B])(f: (A) => M)(g: (B) => M)(implicit F: scalaz.Monoid[M]): M =
    catsBifoldable.bifoldMap(fa)(f, g)

  override def bifoldRight[A, B, C](fa: F[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C = {
    //TODO: is this a correct evaluation laziness?
    def catsF(a: A, c: Eval[C]): Eval[C] =
      Eval.now(f(a, c.value))

    def catsG(b: B, c: Eval[C]): Eval[C] =
      Eval.now(g(b, c.value))

    catsBifoldable.bifoldRight[A, B, C](fa, Eval.later(z))(catsF, catsG).value
  }

}

trait BifoldableConverter {

  implicit def catsToScalazBifoldable[F[_, _]](implicit inner: cats.Bifoldable[F]): scalaz.Bifoldable[F] =
    new ScalazBifoldable[F] with scalaz.Bifoldable[F] {
      override protected implicit val catsBifoldable: cats.Bifoldable[F] = inner
    }

  implicit def catsToScalazBifoldableValue[F[_, _]](inner: cats.Bifoldable[F]): scalaz.Bifoldable[F] =
    catsToScalazBifoldable[F](inner)

}

object BifoldableConverter extends BifoldableConverter

trait ScalazBifunctor[F[_, _]] {
  self: scalaz.Bifunctor[F] =>

  protected implicit def catsBifunctor: cats.functor.Bifunctor[F]

  override def bimap[A, B, C, D](fab: F[A, B])(f: (A) => C, g: (B) => D): F[C, D] =
    catsBifunctor.bimap(fab)(f, g)

}

trait BifunctorConverter {

  implicit def catsToScalazBifunctor[F[_, _]](implicit inner: cats.functor.Bifunctor[F]): scalaz.Bifunctor[F] =
    new ScalazBifunctor[F] with scalaz.Bifunctor[F] {
      override protected implicit val catsBifunctor: cats.functor.Bifunctor[F] = inner
    }

  implicit def catsToScalazBifunctorValue[F[_, _]](inner: cats.functor.Bifunctor[F]): scalaz.Bifunctor[F] =
    catsToScalazBifunctor[F](inner)

}

object BifunctorConverter extends BifunctorConverter

trait ScalazBindRec[F[_]] extends ScalazApply[F] {
  self: scalaz.BindRec[F] =>

  protected implicit def catsFlatMap: cats.FlatMap[F]
  override protected implicit lazy val catsApply: Apply[F] = catsFlatMap

  override def bind[A, B](fa: F[A])(f: (A) => F[B]): F[B] =
    catsFlatMap.flatMap(fa)(s => f(s))

  override def tailrecM[A, B](f: (A) => F[scalaz.Disjunction[A, B]])(a: A): F[B] =
    catsFlatMap.tailRecM(a)((a: A) => catsFlatMap.map(f(a))(_.toEither))
}

trait BindRecConverter {

  implicit def catsToScalazBindRec[F[_]](implicit inner: cats.FlatMap[F]): scalaz.BindRec[F] =
    new ScalazBindRec[F] with scalaz.BindRec[F] {

      override protected implicit val catsFlatMap: cats.FlatMap[F] = inner

      override def xmap[A, B](ma: F[A], f: (A) => B, g: (B) => A): F[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant)
    }

  implicit def catsToScalazBindRecValue[F[_]](inner: cats.FlatMap[F]): scalaz.BindRec[F] =
    catsToScalazBindRec[F](inner)

}

object BindRecConverter extends BindRecConverter

trait BindConverter {

  import BindRecConverter._

  implicit def catsToScalazBind[F[_]](implicit inner: cats.FlatMap[F]): scalaz.Bind[F] =
    catsToScalazBindRec[F]

  implicit def catsToScalazBindValue[F[_]](inner: cats.FlatMap[F]): scalaz.Bind[F] =
    catsToScalazBindRecValue[F](inner)

}

object BindConverter extends BindConverter

trait ScalazCategory[F[_, _]] {
  self: scalaz.Category[F] =>

  protected implicit val catsCategory: cats.arrow.Category[F]

  override def id[A]: F[A, A] =
    catsCategory.id[A]

  override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
    catsCategory.compose(f, g)

}

trait CategoryConverter {

  implicit def catsToscalazCategory[F[_, _]](implicit inner: cats.arrow.Category[F]): scalaz.Category[F] =
    new ScalazCategory[F] with scalaz.Category[F] {
      override protected implicit val catsCategory: cats.arrow.Category[F] = inner
    }

  implicit def catsToscalazCategoryValue[F[_, _], G[_, _], G0[_, _], F0[_, _]](inner: cats.arrow.Category[F]): scalaz.Category[F] =
    catsToscalazCategory[F](inner)

}

object CategoryConverter extends CategoryConverter

trait ScalazCobind[F[_]] extends ScalazFunctor[F] {
  self: scalaz.Cobind[F] =>

  protected implicit def catsCoflatMap: cats.CoflatMap[F]
  override protected implicit lazy val catsFunctor: cats.Functor[F] = catsCoflatMap

  override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B] =
    catsCoflatMap.coflatMap(fa)((x: F[A]) => f(x))

}

trait CoBindConverter {

  implicit def catsToScalazCobind[F[_]](implicit inner: cats.CoflatMap[F]): scalaz.Cobind[F] =
    new ScalazCobind[F] with scalaz.Cobind[F] {

      override protected implicit val catsCoflatMap: cats.CoflatMap[F] = inner

      override def xmap[A, B](fa: F[A], f: (A) => B, g: (B) => A): F[B] =
        ScalazInvariantFunctor.xmap(fa, f, g)(catsInvariant)
    }

  implicit def catsToScalazCobindValue[F[_]](inner: cats.CoflatMap[F]): scalaz.Cobind[F] =
    catsToScalazCobind[F](inner)

}

object CoBindConverter extends CoBindConverter

trait ScalazComonad[F[_]] extends ScalazCobind[F] {
  self: scalaz.Comonad[F] =>

  protected implicit def catsComonad: cats.Comonad[F]
  override protected implicit lazy val catsCoflatMap: cats.CoflatMap[F] = catsComonad

  override def copoint[A](p: F[A]): A =
    catsComonad.extract(p)
}

trait ComonadConverter {

  implicit def catsToScalazComonad[F[_]](implicit inner: cats.Comonad[F]): scalaz.Comonad[F] =
    new ScalazComonad[F] with scalaz.Comonad[F] {

      override protected implicit val catsComonad: cats.Comonad[F] = inner

      override def xmap[A, B](fa: F[A], f: (A) => B, g: (B) => A): F[B] =
        ScalazInvariantFunctor.xmap(fa, f, g)(catsInvariant)
    }

  implicit def catsToScalazComonadValue[F[_]](inner: cats.Comonad[F]): scalaz.Comonad[F] =
    catsToScalazComonad[F](inner)

}

object ComonadConverter extends ComonadConverter

trait ScalazCompose[F[_, _]] {
  self: scalaz.Compose[F] =>

  protected implicit def catsCompose: cats.arrow.Compose[F]

  override def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
    catsCompose.compose(f, g)

}

trait ComposeConverter {

  implicit def catsToScalazCompose[F[_, _]](implicit inner: cats.arrow.Compose[F]): scalaz.Compose[F] =
    new ScalazCompose[F] with scalaz.Compose[F] {
      override protected implicit val catsCompose: cats.arrow.Compose[F] = inner
    }

  implicit def catsToScalazComposeValue[F[_, _], G[_, _], G0[_, _], F0[_, _]](inner: cats.arrow.Compose[F]): scalaz.Compose[F] =
    catsToScalazCompose[F](inner)
}

object ComposeConverter extends ComposeConverter

trait ScalazEqual[F] {
  self: scalaz.Equal[F] =>

  protected implicit def catsEq: cats.Eq[F]

  override def equal(a1: F, a2: F): Boolean =
    catsEq.eqv(a1, a2)
}

trait EqConverter {

  implicit def catsToScalazEqual[F](implicit inner: cats.Eq[F]): scalaz.Equal[F] =
    new ScalazEqual[F] with scalaz.Equal[F] {
      override protected implicit val catsEq: cats.Eq[F] = inner
    }

  implicit def catsToScalazEqualValue[F](inner: cats.Eq[F]): scalaz.Equal[F] =
    catsToScalazEqual[F](inner)

}

object EqConverter extends EqConverter

trait ScalazContravariant[F[_]] {
  self: scalaz.Contravariant[F] =>

  protected implicit def catsContravariant: cats.functor.Contravariant[F]

  override def contramap[A, B](fa: F[A])(f: (B) => A): F[B] =
    catsContravariant.contramap(fa)(f)

}

trait ContravariantConverter {

  implicit def catsToScalazContravariant[F[_]](implicit inner: cats.functor.Contravariant[F]): scalaz.Contravariant[F] =
    new ScalazContravariant[F] with scalaz.Contravariant[F] {
      override protected implicit val catsContravariant: cats.functor.Contravariant[F] = inner

    }

  implicit def catsToScalazContravariantValue[F[_]](inner: cats.functor.Contravariant[F]): scalaz.Contravariant[F] =
    catsToScalazContravariant[F](inner)

}

object ContravariantConverter extends ContravariantConverter

trait ScalazFoldable[F[_]] {
  self: scalaz.Foldable[F] =>

  import harmony.tocats.typeclass.MonoidConverter._
  import cats.Eval

  protected implicit def catsFoldable: cats.Foldable[F]

  override def foldMap[A, B](fa: F[A])(f: (A) => B)(implicit F: scalaz.Monoid[B]): B =
    catsFoldable.foldMap(fa)(f)

  override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B = {
    def catsF(a: A, b: Eval[B]): Eval[B] =
      Eval.later(f(a, b.value))

    catsFoldable.foldRight(fa, Eval.later(z))(catsF).value
  }

}

trait FoldableConverter {

  implicit def catsToScalazFoldable[F[_]](implicit inner: cats.Foldable[F]): scalaz.Foldable[F] =
    new ScalazFoldable[F] with scalaz.Foldable[F] {
      override protected implicit val catsFoldable: cats.Foldable[F] = inner
    }

  implicit def catsToScalazFoldableValue[F[_]](inner: cats.Foldable[F]): scalaz.Foldable[F] =
    catsToScalazFoldable[F](inner)

}

object FoldableConverter extends FoldableConverter

trait ScalazFunctor[F[_]] extends ScalazInvariantFunctor[F] {
  self: scalaz.Functor[F] =>

  protected implicit def catsFunctor: cats.Functor[F]
  override protected implicit lazy val catsInvariant: cats.functor.Invariant[F] = catsFunctor

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] =
    catsFunctor.map[A, B](fa)(f)

}

trait FunctorConverter {

  implicit def catsToScalazFunctor[F[_]](implicit inner: cats.Functor[F]): scalaz.Functor[F] =
    new ScalazFunctor[F] with scalaz.Functor[F] {

      override protected implicit val catsFunctor: cats.Functor[F] = inner

      override def xmap[A, B](ma: F[A], f: (A) => B, g: (B) => A): F[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant)
    }

  implicit def catsToScalazFunctorValue[F[_]](inner: cats.Functor[F]): scalaz.Functor[F] =
    catsToScalazFunctor[F](inner)

}

object FunctorConverter extends FunctorConverter

trait ScalazInvariantFunctor[F[_]] {
  self: scalaz.InvariantFunctor[F] =>

  protected implicit def catsInvariant: cats.functor.Invariant[F]

  override def xmap[A, B](ma: F[A], f: (A) => B, g: (B) => A): F[B] =
    ScalazInvariantFunctor.xmap(ma, f, g)

}

object ScalazInvariantFunctor {
  def xmap[F[_], A, B](ma: F[A], f: (A) => B, g: (B) => A)(implicit inner: cats.functor.Invariant[F]): F[B] = {
    inner.imap(ma)(f)(g)
  }
}

trait InvariantFunctorConverter {

  implicit def catsToScalazInvariantFunctorInstance[F[_]](implicit inner: cats.functor.Invariant[F]): scalaz.InvariantFunctor[F] =
    new ScalazInvariantFunctor[F] with scalaz.InvariantFunctor[F] {

      override protected implicit val catsInvariant: cats.functor.Invariant[F] = inner
    }

  implicit def catsToScalazInvariantFunctorValue[F[_]](inner: cats.functor.Invariant[F]): scalaz.InvariantFunctor[F] =
    catsToScalazInvariantFunctorInstance(inner)

}

object InvariantFunctorConverter extends InvariantFunctorConverter

trait ScalazMonad[F[_]]
  extends ScalazApplicative[F]
    with ScalazBindRec[F] {
  self: scalaz.Monad[F] with scalaz.BindRec[F] =>

  protected implicit def catsMonad: cats.Monad[F]
  override protected implicit lazy val catsApplicative: cats.Applicative[F] = catsMonad
  override protected implicit lazy val catsFlatMap: cats.FlatMap[F] = catsMonad
}

trait MonadConverter {

  implicit def catsToScalazMonad[F[_]](implicit inner: cats.Monad[F]): scalaz.Monad[F] =
    new ScalazMonad[F] with scalaz.Monad[F] with scalaz.BindRec[F] {

      override protected implicit val catsMonad: cats.Monad[F] = inner

      override def xmap[A, B](ma: F[A], f: (A) => B, g: (B) => A): F[B] =
        ScalazInvariantFunctor.xmap[F, A, B](ma, f, g)(catsInvariant)
    }

  implicit def catsToScalazMonadValue[F[_]](inner: cats.Monad[F]): scalaz.Monad[F] =
    catsToScalazMonad[F](inner)

}

object MonadConverter extends MonadConverter

trait ScalazMonadError[F[_], S]
  extends ScalazMonad[F] {
  self: scalaz.MonadError[F, S] with scalaz.BindRec[F] =>

  protected implicit def catsMonadError: cats.MonadError[F, S]
  override protected implicit lazy val catsMonad: cats.Monad[F] = catsMonadError

  override def raiseError[A](e: S): F[A] =
    catsMonadError.raiseError(e)

  override def handleError[A](fa: F[A])(f: (S) => F[A]): F[A] =
    catsMonadError.handleErrorWith(fa)(s => f(s))

}

trait MonadErrorConverter {

  implicit def catsToScalazMonadError[F[_], S](implicit inner: cats.MonadError[F, S]): scalaz.MonadError[F, S] =
    new ScalazMonadError[F, S]
      with scalaz.MonadError[F, S]
      with scalaz.BindRec[F] {

      override protected implicit val catsMonadError: cats.MonadError[F, S] = inner

      override def xmap[A, B](ma: F[A], f: (A) => B, g: (B) => A): F[B] =
        ScalazInvariantFunctor.xmap(ma, f, g)(catsInvariant)
    }

  implicit def catsToScalazMonadErrorValue[F[_], S](inner: cats.MonadError[F, S]): scalaz.MonadError[F, S] =
    catsToScalazMonadError[F, S](inner)

}

object MonadErrorConverter extends MonadErrorConverter

trait ScalazMonoid[F] {
  self: scalaz.Monoid[F] =>

  protected implicit def catsMonoid: cats.Monoid[F]

  override def zero: F =
    catsMonoid.empty

  override def append(f1: F, f2: => F): F =
    catsMonoid.combine(f1, f2)
}

trait MonoidConverter {

  implicit def catsToScalazMonoid[F](implicit inner: cats.Monoid[F]): scalaz.Monoid[F] =
    new ScalazMonoid[F] with scalaz.Monoid[F] {
      override protected implicit val catsMonoid: cats.Monoid[F] = inner
    }

  implicit def catsToScalazMonoidValue[F](inner: cats.Monoid[F]): scalaz.Monoid[F] =
    catsToScalazMonoid[F](inner)

}

object MonoidConverter extends MonoidConverter

trait ScalazFunctionK[F[_], G[_]] {
  self: scalaz.NaturalTransformation[F, G] =>

  protected implicit def catsFunctionK: cats.arrow.FunctionK[F, G]

  override def apply[A](fa: F[A]): G[A] =
    catsFunctionK(fa)

}

trait FunctionKConverter {

  implicit def catsToScalazNaturalTransformation[F[_], G[_]](implicit inner: cats.arrow.FunctionK[F, G]): scalaz.NaturalTransformation[F, G] =
    new ScalazFunctionK[F, G] with scalaz.NaturalTransformation[F, G] {
      override protected implicit val catsFunctionK: cats.arrow.FunctionK[F, G] = inner
    }

  implicit def catsToScalazNaturalTransformationValue[F[_], G[_]](inner: cats.arrow.FunctionK[F, G]): scalaz.NaturalTransformation[F, G] =
    catsToScalazNaturalTransformation[F, G](inner)
}

object FunctionKConverter extends FunctionKConverter

trait ScalazOrder[F, G] {
  self: scalaz.Order[F] =>

  protected implicit def catsToScalazOrderingEQ(inner: cats.kernel.Comparison.EqualTo.type): scalaz.Ordering.EQ.type =
    scalaz.Ordering.EQ

  protected implicit def catsToScalazOrderingGT(inner: cats.kernel.Comparison.GreaterThan.type): scalaz.Ordering.GT.type =
    scalaz.Ordering.GT

  protected implicit def catsToScalazOrderingLT(inner: cats.kernel.Comparison.LessThan.type): scalaz.Ordering.LT.type =
    scalaz.Ordering.LT

  protected implicit def catsToScalazOrdering(inner: cats.kernel.Comparison): scalaz.Ordering =
    scalaz.Ordering.fromInt(inner.toInt)

  protected implicit def catsOrder: cats.Order[F]

  override def order(x: F, y: F): scalaz.Ordering =
    catsOrder.comparison(x, y)

}

trait OrderConverter {

  implicit def catsToScalazOrder[F, G](implicit inner: cats.Order[F]): scalaz.Order[F] =
    new ScalazOrder[F, G] with scalaz.Order[F] {
      override protected implicit val catsOrder: cats.Order[F] = inner
    }

  implicit def catsToScalazOrderValue[F, G](inner: cats.Order[F]): scalaz.Order[F] =
    catsToScalazOrder[F, G](inner)

}

object OrderConverter extends OrderConverter

trait ScalazSemigroup[F] {
  self: scalaz.Semigroup[F] =>

  protected implicit def catsSemigroup: cats.Semigroup[F]

  override def append(f1: F, f2: => F): F =
    catsSemigroup.combine(f1, f2)

}

trait SemigroupConverter {

  implicit def catsToScalazSemigroup[F](implicit inner: cats.Semigroup[F]): scalaz.Semigroup[F] =
    new ScalazSemigroup[F] with scalaz.Semigroup[F] {
      override protected implicit val catsSemigroup: cats.Semigroup[F] = inner
    }

  implicit def catsToScalazSemigroupValue[F](inner: cats.Semigroup[F]): scalaz.Semigroup[F] =
    catsToScalazSemigroup[F](inner)

}

object SemigroupConverter extends SemigroupConverter

trait ScalazShow[F] {
  self: scalaz.Show[F] =>

  protected implicit def catsShow: cats.Show[F]

}

object ScalazShow {
  def shows[F](f: F)(implicit inner: cats.Show[F]): String =
    inner.show(f)
}

trait ShowConverter {

  implicit def catsToScalazShow[F](implicit inner: cats.Show[F]): scalaz.Show[F] =
    new ScalazShow[F] with scalaz.Show[F] {
      override protected implicit val catsShow: cats.Show[F] = inner
      override def shows(f: F): String =
        catsShow.show(f)
    }

  implicit def catsToScalazShowValue[F](inner: cats.Show[F]): scalaz.Show[F] =
    catsToScalazShow[F](inner)

}

object ShowConverter extends ShowConverter

trait ScalazTraverse[F[_]] {
  self: scalaz.Traverse[F] =>

  import harmony.tocats.typeclass.ApplicativeConverter._

  protected implicit def catsTraverse: cats.Traverse[F]

  override def traverseImpl[T[_]: scalaz.Applicative, A, B](fa: F[A])(f: (A) => T[B]): T[F[B]] = {
    catsTraverse.traverse(fa)(f)
  }

}

trait TraverseConverter {

  implicit def catsToScalazTraverse[F[_]](implicit inner: cats.Traverse[F]): scalaz.Traverse[F] =
    new ScalazTraverse[F] with scalaz.Traverse[F] {

      override protected implicit val catsTraverse: cats.Traverse[F] = inner
    }

  implicit def catsToScalazTraverseValue[F[_]](inner: cats.Traverse[F]): scalaz.Traverse[F] =
    catsToScalazTraverse[F](inner)

}

object TraverseConverter extends TraverseConverter
