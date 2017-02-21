package harmony.tocats

import harmony.tocats.typeclass._

trait TypeClass
  extends AlternativeConverter
    with ApplicativeConverter
    with ApplyConverter
    with ArrowConverter
    with BifoldableConverter
    with BifunctorConverter
    with BindConverter
    with CategoryConverter
//    with ChoiceConverter
    with CoBindConverter
    with ComonadConverter
    with ComposeConverter
    with ContravariantConverter
    with EqualConverter
    with FoldableConverter
    with FunctorConverter
    with InvariantFunctorConverter
    with MonadConverter
    with MonadErrorConverter
    with MonadReaderConverter
    with MonadStateConverter
    with MonoidConverter
    with NaturalTransformationConverter
    with OrderConverter
    with ShowConverter
    with TraverseConverter

object TypeClass
  extends TypeClass
