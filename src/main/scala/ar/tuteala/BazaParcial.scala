package ar.tuteala

import ar.tuteala.TuteUtils._

case class BazaParcial(jugadas: List[(Jugador, Carta)], triunfo: Palo) extends Baza {

  def conUnaJugadaMas(bazasAnteriores: List[BazaCompleta], siguienteJugador: Jugador, manoDelSiguienteJugador: List[Carta]): BazaParcial = {
    val nuevaJugada = siguienteJugador.jugar(bazasAnteriores, this, manoDelSiguienteJugador, jugables(manoDelSiguienteJugador))
    val jugadasConLaNueva = jugadas :+ nuevaJugada

    BazaParcial(jugadasConLaNueva, triunfo)
  }

  // este es el fold interior.
  def jugar(bazasAnteriores: List[BazaCompleta], jugadoresConManos: JugadoresConCartas): BazaCompleta = {
    val baza = jugadoresConManos.foldLeft(this) { case (baza, (jugador, mano)) => baza.conUnaJugadaMas(bazasAnteriores, jugador, mano) }
    baza.completaConCanto(bazasAnteriores, baza.jugadoresConManosDespues(jugadoresConManos))
  }

  def jugadoresConManosDespues(jugadoresConManosAntes: JugadoresConCartas): JugadoresConCartas =
    // asume que 2 jugadores no pueden haber jugado la misma carta (no pasa en otros juegos)
    jugadoresConManosAntes map { case (jugador, mano) => (jugador, mano sin cartasEnMesa) }

  def cartasEnMesa: List[Carta] =
    jugadas map (_._2)

  def completaConCanto(bazasAnteriores: List[BazaCompleta], jugadoresConManos: JugadoresConCartas): BazaCompleta = {
    val canto = levantador.cantar(bazasAnteriores, this, manoDelLevantador(jugadoresConManos), cantables(manoDelLevantador(jugadoresConManos)))
    BazaCompleta(jugadoresConManos, jugadas, triunfo, canto)
  }

  def jugables(mano: List[Carta]): List[Carta] =
    mano jugables this

  def cantables(mano: List[Carta]): List[Canto] =
    Mazo.cantos filter (_.esCantable(mano, triunfo))
}

object BazaParcial {
  def nueva(triunfo: Palo): BazaParcial = BazaParcial(Nil, triunfo)
}