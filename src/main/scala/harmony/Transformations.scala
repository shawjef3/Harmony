package harmony

/**
  * Scalaz's NaturalTransformation, but refl is implicit.
  * @tparam F
  * @tparam G
  */
trait NaturalTransformation[-F[_], +G[_]] {
  outer =>

  def apply[A](f: F[A]): G[A]

  def compose[E[_]](f: NaturalTransformation[E, F]): NaturalTransformation[E, G] = λ[NaturalTransformation[E, G]](
    ea => outer(f(ea))
  )

  def andThen[H[_]](f: NaturalTransformation[G, H]): NaturalTransformation[F, H] =
    f compose outer

  lazy val toScalaz: scalaz.NaturalTransformation[F, G] =
    new scalaz.NaturalTransformation[F, G] {
      override def apply[A](fa: F[A]): G[A] = outer(fa)
    }

  //Cats' FunctionK is invariant, so there is no conversion.

}

object NaturalTransformation {
  def apply[F[_], G[_]](implicit n: NaturalTransformation[F, G]): NaturalTransformation[F, G] = n

  implicit def refl[F[_]]: NaturalTransformation[F, F] =
    new NaturalTransformation[F, F] {
      override def apply[A](f: F[A]): F[A] = f
    }

  def fromCats[F[_], G[_]](implicit n: cats.arrow.FunctionK[F, G]): NaturalTransformation[F, G] =
    new NaturalTransformation[F, G] {
      override def apply[A](f: F[A]): G[A] = n(f)
    }

  def fromScalaz[F[_], G[_]](implicit n: scalaz.NaturalTransformation[F, G]): NaturalTransformation[F, G] =
    new NaturalTransformation[F, G] {
      override def apply[A](f: F[A]): G[A] = n(f)
    }

}

/**
  *
  * @tparam F
  * @tparam G
  * @tparam G0 contravariant G
  * @tparam F0 covariant F
  */
trait ReversableNatTrans[-F[_], +G[_], -G0[_], +F0[_]]
  extends NaturalTransformation[F, G] {

  val reverse: ReversableNatTrans[G0, F0, F, G]

}

object ReversableNatTrans {

  def apply[F[_], G[_], G0[_], F0[_]](implicit r: ReversableNatTrans[F, G, G0, F0]): ReversableNatTrans[F, G, G0, F0] = r

  /**
    * The same as .reverse, but with the original type parameters.
    */
  def reverse[F[_], G[_], G0[_], F0[_]](r: ReversableNatTrans[F, G, G0, F0]): ReversableNatTrans[G, F, F0, G0] =
    r.reverse.asInstanceOf[ReversableNatTrans[G, F, F0, G0]]

  implicit def refl[F[_]]: ReversableNatTrans[F, F, F, F] = {
    new ReversableNatTrans[F, F, F, F] {
      override def apply[A](fa: F[A]): F[A] = fa

      override val reverse: ReversableNatTrans[F, F, F, F] = this
    }
  }

  implicit def fromTransformations[F[_], G[_], G0[_], F0[_]](implicit n0: NaturalTransformation[F, G], n1: NaturalTransformation[G0, F0]): ReversableNatTrans[F, G, G0, F0] =
    new ReversableNatTrans[F, G, G0, F0] {
      outer =>

      override def apply[A](fa: F[A]): G[A] =
        n0.apply[A](fa)

      override val reverse: ReversableNatTrans[G0, F0, F, G] =
        new ReversableNatTrans[G0, F0, F, G] {
          override val reverse: ReversableNatTrans[F, G, G0, F0] = outer

          override def apply[A](fa: G0[A]): F0[A] = n1.apply(fa)
        }
    }

  def fromCats[F[_], G[_]](implicit n0: cats.arrow.FunctionK[F, G], n1: cats.arrow.FunctionK[G, F]): ReversableNatTrans[F, G, G, F] =
    new ReversableNatTrans[F, G, G, F] {
      outer =>

      override def apply[A](f: F[A]): G[A] = n0(f)

      override val reverse: ReversableNatTrans[G, F, F, G] =
        new ReversableNatTrans[G, F, F, G] {
          override val reverse: ReversableNatTrans[F, G, G, F] = outer

          override def apply[A](f: G[A]): F[A] = n1(f)
        }
    }

  def fromScalaz[F[_], G[_]](implicit n0: scalaz.NaturalTransformation[F, G], n1: scalaz.NaturalTransformation[G, F]): ReversableNatTrans[F, G, G, F] =
    new ReversableNatTrans[F, G, G, F] {
      outer =>

      override def apply[A](f: F[A]): G[A] = n0(f)

      override val reverse: ReversableNatTrans[G, F, F, G] =
        new ReversableNatTrans[G, F, F, G] {
          override val reverse: ReversableNatTrans[F, G, G, F] = outer

          override def apply[A](f: G[A]): F[A] = n1(f)
        }
    }

}

/**
  * Scalaz's BiNaturalTransformation, but refl is implicit.
  * @tparam F
  * @tparam G
  */
trait BiNaturalTransformation[-F[_, _], +G[_, _]] {
  outer =>

  def apply[A, B](f: F[A, B]): G[A, B]

  def compose[E[_, _]](f: BiNaturalTransformation[E, F]) =
    new BiNaturalTransformation[E, G] {
      def apply[A, B](eab: E[A, B]): G[A, B] = outer(f(eab))
    }

  lazy val toScalaz: scalaz.BiNaturalTransformation[F, G] =
    new scalaz.BiNaturalTransformation[F, G] {
      override def apply[A, B](fa: F[A, B]): G[A, B] = outer(fa)
    }

  //cats has no BiFunctionK, so there is no conversion.

}

object BiNaturalTransformation {
  def apply[F[_, _], G[_, _]](implicit n: BiNaturalTransformation[F, G]): BiNaturalTransformation[F, G] = n

  implicit def refl[F[_, _]]: BiNaturalTransformation[F, F] =
    new BiNaturalTransformation[F, F] {
      override def apply[A, B](f: F[A, B]): F[A, B] = f
    }
}

trait ReversableBiNatTrans[-F[_, _], +G[_, _], -G0[_, _], +F0[_, _]]
  extends BiNaturalTransformation[F, G] {

  val reverse: ReversableBiNatTrans[G0, F0, F, G]

}

object ReversableBiNatTrans {

  def apply[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit r: ReversableBiNatTrans[F, G, G0, F0]): ReversableBiNatTrans[F, G, G0, F0] = r

  /**
    * The same as .reverse, but with the original type parameters.
    */
  def reverse[F[_, _], G[_, _], G0[_, _], F0[_, _]](r: ReversableBiNatTrans[F, G, G0, F0]): ReversableBiNatTrans[G, F, F0, G0] =
    r.reverse.asInstanceOf[ReversableBiNatTrans[G, F, F0, G0]]

  implicit def fromTransformations[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit n0: BiNaturalTransformation[F, G], n1: BiNaturalTransformation[G0, F0]): ReversableBiNatTrans[F, G, G0, F0] =
    new ReversableBiNatTrans[F, G, G0, F0] {
      outer =>

      override def apply[A, B](fa: F[A, B]): G[A, B] =
        n0.apply[A, B](fa)

      override val reverse: ReversableBiNatTrans[G0, F0, F, G] =
        new ReversableBiNatTrans[G0, F0, F, G] {
          override val reverse: ReversableBiNatTrans[F, G, G0, F0] = outer

          override def apply[A, B](f: G0[A, B]): F0[A, B] =
            n1(f)
        }
    }

  implicit def refl[F[_, _]]: ReversableBiNatTrans[F, F, F, F] =
    new ReversableBiNatTrans[F, F, F, F] {
      override def apply[A, B](f: F[A, B]): F[A, B] = f

      override val reverse: ReversableBiNatTrans[F, F, F, F] = this
    }

  def fromScalaz[F[_, _], G[_, _], G0[_, _], F0[_, _]](implicit n0: scalaz.BiNaturalTransformation[F, G], n1: scalaz.BiNaturalTransformation[G0, F0]): ReversableBiNatTrans[F, G, G0, F0] =
    new ReversableBiNatTrans[F, G, G0, F0] {
      outer =>

      override def apply[A, B](f: F[A, B]): G[A, B] = n0(f)

      override val reverse: ReversableBiNatTrans[G0, F0, F, G] =
        new ReversableBiNatTrans[G0, F0, F, G] {
          override val reverse: ReversableBiNatTrans[F, G, G0, F0] = outer

          override def apply[A, B](f: G0[A, B]): F0[A, B] = n1(f)
        }
    }

}
