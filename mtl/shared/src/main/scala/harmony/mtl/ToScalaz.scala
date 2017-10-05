package harmony.mtl

trait ToScalaz
  extends harmony.mtl.toscalaz.MonadStateConverter

object ToScalaz extends ToScalaz
