package ar.tuteala

import ar.tuteala.TuteUtils._

case object EstrategiaDePrueba extends Estrategia {
  override def jugar(bazasAnteriores: List[BazaCompleta], bazaActual: BazaParcial, manoPropia: List[Carta], jugables: List[Carta]): Carta =
    jugables.head //for testing

  override def cantar(bazasAnteriores: List[BazaCompleta], bazaActual: BazaParcial, manoPropia: List[Carta], cantables: List[Canto]): Canto =
    NoCanto
}