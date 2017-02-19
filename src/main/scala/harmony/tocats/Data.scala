package harmony.tocats

import harmony.tocats.data._

trait Data
  extends CoproductConverter
    with CoyonedaConverter
    with DisjunctionConverter
    with EitherTConverter
    with IdTConverter
    with IListConverter
    with KleisliConverter
    with MaybeConverter
    with MaybeTConverter
    with NameConverter
    with NonEmptyListConverter
    with OneAndConverter
    with OptionTConverter
    with OrderingConverter
    with StateTConverter
    with ValidationConverter
    with ValidationNelConverter
    with WriterTConverter
    with YonedaConverter

object Data
  extends Data
