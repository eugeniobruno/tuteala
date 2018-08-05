package ar.tuteala

abstract class Estrategia {
  def jugar(bazasAnteriores: List[BazaCompleta], bazaActual: BazaParcial, manoPropia: List[Carta], jugables: List[Carta]): Carta
  def cantar(bazasAnteriores: List[BazaCompleta], bazaActual: BazaParcial, manoPropia: List[Carta], cantables: List[Canto]): Canto
}