package harmony

import harmony.tocats._

trait ToCats
  extends Data
  with TypeClass

object ToCats
  extends ToCats
