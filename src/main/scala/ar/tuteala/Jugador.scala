package ar.tuteala

import ar.tuteala.TuteUtils._

case class Jugador(nombre: String, estrategia: Estrategia = EstrategiaDePrueba) {
  override def toString: String = nombre

  def jugar(bazasAnteriores: List[BazaCompleta], bazaActual: BazaParcial, manoPropia: List[Carta], jugables: List[Carta]): (Jugador, Carta) = {
    //println("mano de " + this + ": " + manoPropia.toString)
    (this, jugarCarta(bazasAnteriores, bazaActual, manoPropia, jugables))
  }

  // cartaJugada? nuevaJugada?
  def jugarCarta(bazasAnteriores: List[BazaCompleta], bazaActual: BazaParcial, manoPropia: List[Carta], jugables: List[Carta]): Carta = {
    val carta = estrategia.jugar(bazasAnteriores, bazaActual, manoPropia, jugables)
    if (jugables contains carta) carta else throw new RuntimeException("TODO jugada invalida")
  }

  def cantar(bazasAnteriores: List[BazaCompleta], bazaActual: BazaParcial, manoPropia: List[Carta], cantables: List[Canto]): Canto = {
    val canto = estrategia.cantar(bazasAnteriores, bazaActual, manoPropia, cantables)
    if (cantables contains canto) canto else throw new RuntimeException("TODO canto invalido")
  }

}