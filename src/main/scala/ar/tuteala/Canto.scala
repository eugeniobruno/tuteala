package ar.tuteala

import ar.tuteala.TuteUtils._

sealed trait Canto {
  def puntos: Int
  def esCantable(mano: List[Carta], triunfo: Palo): Boolean
}

final case class LasVeinteEn(palo: Palo) extends Canto {
  def puntos: Int = 20
  def esCantable(mano: List[Carta], triunfo: Palo) =
    (palo != triunfo) && (mano tieneOnceYDoceDe palo)
}

final case object LasCuarenta extends Canto {
  def puntos: Int = 40
  def esCantable(mano: List[Carta], triunfo: Palo) =
    mano tieneOnceYDoceDe triunfo
}

final case object NoCanto extends Canto {
  def puntos: Int = 0
  def esCantable(mano: List[Carta], triunfo: Palo) =
    true
}

final case object Tute extends Canto {
  def puntos: Int = throw new RuntimeException("Prohibido preguntarle los puntos al canto Tute")
  def esCantable(mano: List[Carta], triunfo: Palo) =
    (mano tieneTodosLos Once) || (mano tieneTodosLos Doce)
}
