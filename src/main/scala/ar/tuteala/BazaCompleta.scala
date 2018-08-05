package ar.tuteala

import ar.tuteala.TuteUtils._

case class BazaCompleta (jugadoresConManos: JugadoresConCartas, jugadas: List[(Jugador, Carta)], triunfo: Palo, canto: Canto) extends Baza {

  def jugadores: List[Jugador] =
    jugadoresConManos map (_._1)

  def levantadorConLevantadasYCantos: (Jugador, List[Carta], List[Canto]) =
    (levantador, cartas, List(canto))

  def levantadorConSuMano: (Jugador, List[Carta]) =
    (levantador, manoDelLevantador(jugadoresConManos))

}