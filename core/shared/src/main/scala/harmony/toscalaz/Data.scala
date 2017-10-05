package harmony.toscalaz

import harmony.toscalaz.data._

trait Data
  extends ComparisonConverter
    with CoproductConverter
    with CoyonedaConverter
    with EitherConverter
    with EitherTConverter
    with EvalConverter
    with IdTConverter
    with KleisliConverter
    with NonEmptyListConverter
    with NonEmptyVectorConverter
    with OneAndConverter
    with OptionToMaybeConverter
    with OptionTToMaybeTConverter
    with OptionTToOptionTConverter
    with StateTConverter
    with ValidatedConverter
    with ValidatedNelConverter
    with WriterTConverter
    with YonedaConverter

object Data
  extends Data
