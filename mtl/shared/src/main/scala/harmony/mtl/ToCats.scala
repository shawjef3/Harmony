package harmony.mtl

trait ToCats
  extends harmony.mtl.tocats.ApplicativeLocalConverter
  with harmony.mtl.tocats.MonadStateConverter

object ToCats extends ToCats
