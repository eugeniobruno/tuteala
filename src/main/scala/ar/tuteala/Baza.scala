package ar.tuteala

import ar.tuteala.TuteUtils._

abstract class Baza {

  val jugadas: List[(Jugador, Carta)]
  val triunfo: Palo

  def cartas: List[Carta] =
    jugadas map (_._2)

  val primerPalo: Option[Palo] = cartas match {
    case Carta(_, palo) :: _ => Some(palo)
    case Nil => None
  }

  def esLevantadora(carta: Carta): Boolean =
    Some(carta) == cartaLevantadora

  def esLevantadora(jugada: (Jugador, Carta)): Boolean =
    esLevantadora(jugada._2)

  def seriaLevantadora(carta: Carta): Boolean =
    carta.mata(cartaLevantadora.toList, primerPalo, triunfo)

  val cartaLevantadora: Option[Carta] =
    cartas.laQueMataTodasLasDemas(primerPalo, triunfo)

  def levantadora: (Jugador, Carta) =
    jugadas.find(esLevantadora)
      .getOPincha("Estado imposible: Ninguna carta de la baza es levantadora")

  def levantador: Jugador =
    levantadora._1

  def manoDelLevantador(jugadoresConManos: JugadoresConCartas): List[Carta] =
    manoDe(levantador, jugadoresConManos)

  def manoDe(jugador: Jugador, jugadoresConManos: JugadoresConCartas): List[Carta] = {
    jugadoresConManos.toMap.get(jugador).getOPincha("No se encontro la mano de " + jugador)
  }

}