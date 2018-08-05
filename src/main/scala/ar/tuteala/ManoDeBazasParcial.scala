package ar.tuteala

import ar.tuteala.TuteUtils._

case class ManoDeBazasParcial(bazasCompletas: List[BazaCompleta], jugadoresConManos: List[(Jugador, List[Carta])], triunfo: Palo, opcionesDePartido: OpcionesDePartido) {

  def continuada: ManoDeBazasParcial =
    bazasCompletas match {
      case Nil => conUnaBazaMas
      case _ => bazasCompletas.last match {
        case BazaCompleta(_, _, _, Tute) => opcionesDePartido.valorDeTute match {
          case ElRestoPierde | Capotes(_) => this
          case Nada => conUnaBazaMas
      }
        case _ => conUnaBazaMas
    }
  }

  def conUnaBazaMas: ManoDeBazasParcial = {
    val nuevaBazaCompleta = BazaParcial.nueva(triunfo).jugar(bazasCompletas, jugadoresConManos)
    val bazasCompletasConLaNueva = bazasCompletas :+ nuevaBazaCompleta
    val jugadoresConManosReordenados = nuevaBazaCompleta.jugadoresConManos.elPrimeroSiendo(nuevaBazaCompleta.levantadorConSuMano)

    ManoDeBazasParcial(bazasCompletasConLaNueva, jugadoresConManosReordenados, triunfo, opcionesDePartido)
  }

  def jugar: ManoDeBazasCompleta = {
    //este es el fold del medio.
    val cantidadDeJugadores = jugadoresConManos.length
    val cantidadDeCartas = (jugadoresConManos flatMap (_._2)).length
    val cantidadDeBazasPorMano = cantidadDeCartas / cantidadDeJugadores
    val manoDeBazasParcial = (1 to cantidadDeBazasPorMano).foldLeft(this) ((manoDeBazas: ManoDeBazasParcial, turno: Int) => manoDeBazas.continuada)
    manoDeBazasParcial.completa
  }

  def completa: ManoDeBazasCompleta =
    // TODO if jugadoresConManos tiene alguna lista distinta de nil throw exception
    //ManoDeBazasCompleta(bazasCompletas, opcionesDePartido)
    ManoDeBazasCompleta.luegoDe(bazasCompletas, opcionesDePartido)
}

object ManoDeBazasParcial {
  def nueva(jugadoresConManos: List[(Jugador, List[Carta])], triunfo: Palo, opcionesDePartido: OpcionesDePartido): ManoDeBazasParcial =
    ManoDeBazasParcial(Nil, jugadoresConManos, triunfo, opcionesDePartido)
}