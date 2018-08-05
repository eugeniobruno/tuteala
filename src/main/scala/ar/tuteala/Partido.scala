package ar.tuteala

import ar.tuteala.TuteUtils._

case class Partido(jugadores: List[Jugador], manos: List[ManoDeBazasCompleta], jugadoresConManos: List[(Jugador, List[Carta])], triunfo: Palo, resultado: ResultadoDePartido, opciones: OpcionesDePartido = OpcionesDePartido()) {

  override def toString: String = {
    val intro: String =
      if (termino)
        "Partido completo. Resultado final: "
      else
        "Partido en juego. Resultado parcial: "
    intro + resultado.toString()
  }

  def nuevaManoDeBazasCompleta: ManoDeBazasCompleta =
    ManoDeBazasParcial.nueva(jugadoresConManos, triunfo, opciones).jugar

  def triunfoSiguiente: Palo =
    triunfo.siguiente

  def continuado: Partido =
    if (termino) this else conUnaManoMas

  def conUnaManoMas: Partido = {
    val nuevaMano = this.nuevaManoDeBazasCompleta //TODO probar un lazy val
    val listaDeManosDeBazasCompletasActualizada = manos :+ nuevaMano
    val jugadoresReordenados = jugadores.rotadaUnaPosicion
    val jugadoresConManosNuevasReordenados = Partido.jugadoresConManosNuevas(jugadoresReordenados, triunfoSiguiente)
    val resultadoActualizado = resultado.sumadoCon(nuevaMano.resultado, opciones.aCuanto)

    Partido(jugadoresReordenados, listaDeManosDeBazasCompletasActualizada, jugadoresConManosNuevasReordenados, triunfoSiguiente, resultadoActualizado, opciones)
  }

  def jugar: Partido = {
    //este es el fold exterior
    val cantidadMaximaDeManos = jugadores.length * (opciones.aCuanto - 1) + 1
    (1 to cantidadMaximaDeManos).foldLeft(this) ((partido, numMano) => partido.continuado)
  }

  def termino: Boolean =
    resultado.esFinal(opciones.aCuanto)

  def perdedores: List[Jugador] =
    resultado.perdedores(opciones.aCuanto)
}

object Partido {

  def jugadoresConManosNuevas(jugadores: List[Jugador], triunfo: Palo): List[(Jugador, List[Carta])] = {
    val cantidadDeJugadores = jugadores.length
    val mazoDivisible = cantidadDeJugadores match {
      case 3 => Mazo.mezclado sin Carta(Dos, triunfo)
      case 4 | 5 => Mazo.mezclado
      case _ => throw new RuntimeException("No se puede jugar de a " + cantidadDeJugadores)
    }
    val mazoMezcladoNumerado = mazoDivisible.zipWithIndex
    val jugadoresNumerados = jugadores.zipWithIndex
    val manoNueva: Int => List[Carta] = (nj =>
      mazoMezcladoNumerado collect {
        case (c, nc) if nc % cantidadDeJugadores == nj => c
      }
    )
    jugadoresNumerados map { case (j, nj) => (j, manoNueva(nj)) }
  }

  def nuevo(jugadores: List[Jugador], opciones: OpcionesDePartido = OpcionesDePartido()): Partido = {
    val jugadoresConPuntos = jugadores map ((_, 0))
    val resultadoInicial = ResultadoDePartido(jugadoresConPuntos)
    Partido(jugadores, Nil, jugadoresConManosNuevas(jugadores, Oro), Oro, resultadoInicial, opciones)
  }

}