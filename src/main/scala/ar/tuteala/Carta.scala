package ar.tuteala

import ar.tuteala.TuteUtils._

case class Carta (numero: Numero, palo: Palo) {
  override def toString = numero + " de " + palo

  def puntos: Int = numero.puntos
  def sumaPuntos: Boolean = this.puntos > 0

  /** Devuelve `true` si esta `Carta` mata a todas las `otras`. */
  def mata(otras: List[Carta], primerPalo: Option[Palo], triunfo: Palo): Boolean =
    otras forall (this.mata(_, primerPalo, triunfo))

  /** Devuelve `true` si esta `Carta` mata a `otra`. */
  protected def mata(otra: Carta, primerPalo: Option[Palo], triunfo: Palo): Boolean =
    (this.palo, otra.palo, primerPalo, triunfo) match {
      case (paloPropio, paloAgeno, _, _) if paloPropio == paloAgeno => this.mataMismoPalo(otra)
      // las cartas son de distinto palo
      case (paloPropio, _, _, triunfo) if paloPropio == triunfo => true
      case (_, paloAgeno, _, triunfo) if paloAgeno == triunfo => false
      // ninguna carta es del palo del triunfo
      case (paloPropio, _, Some(primerPalo), _) if paloPropio == primerPalo => true
      case (_, paloAgeno, Some(primerPalo), _) if paloAgeno == primerPalo => false
      // ninguna carta es del primer palo de la baza
      case _ => false
    }

  /** Devuelve `true` si esta `Carta` mata a `otra`.
    *
    * Asume que ambas cartas son del mismo `Palo`.
    */
  protected def mataMismoPalo(otra: Carta): Boolean =
    this.numero > otra.numero
}