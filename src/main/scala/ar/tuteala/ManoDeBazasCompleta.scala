package ar.tuteala

import ar.tuteala.TuteUtils._

case class ManoDeBazasCompleta(jugadoresConLevantadasYCantos: Map[Jugador, (List[Carta], List[Canto])], ultimoLevantador: Jugador, ultimoCanto: Canto, opcionesDePartido: OpcionesDePartido) {

  def resultado: ResultadoDePartido = cantorDeTute match {
    case Some(cantor) => opcionesDePartido.valorDeTute match {
      case ElRestoPierde => todosPierdenExcepto(cantor)
      case Capotes(n) => todosSumanExcepto(n, cantor)
      case Nada => resultadoMasSimple
    }
    case None => resultadoMasSimple
  }

  protected def cantorDeTute: Option[Jugador] =
    ultimoCanto match {
      case Tute => Some(ultimoLevantador)
      case _ => None
    }

  protected def todosPierdenExcepto(ganador: Jugador): ResultadoDePartido =
    ResultadoDePartido.suman(opcionesDePartido.aCuanto, jugadores sin ganador)

  protected def todosSumanExcepto(puntos: Int, jugador: Jugador) =
    ResultadoDePartido.suman(puntos, jugadores sin jugador)

  protected val jugadores: List[Jugador] =
    jugadoresConLevantadasYCantos.keySet.toList

  protected def resultadoMasSimple: ResultadoDePartido =
    ResultadoDePartido sumanUno perdedores

  protected def perdedores: List[Jugador] =
    DeterminadorDePerdedores(
      jugadoresConLevantadasYCantos,
      ultimoLevantador
    ).perdedores

}

object ManoDeBazasCompleta {
  def luegoDe(bazasCompletas: List[BazaCompleta], opcionesDePartido: OpcionesDePartido): ManoDeBazasCompleta = {
    val ultimaBaza = bazasCompletas.last
    val jugadores = ultimaBaza.jugadores
    val ultimoLevantador = ultimaBaza.levantador
    val ultimoCanto = ultimaBaza.canto

    val todosSinLevantadasNiCantos = jugadores map ((_, Nil, Nil))
    val levantadoresConLevantadasYCantos = bazasCompletas map (_.levantadorConLevantadasYCantos)

    val jugadoresConLevantadasYCantos =
      (todosSinLevantadasNiCantos ++ levantadoresConLevantadasYCantos)
      .groupBy (_._1)
      .mapValues (_
        .map (_.sin_1)
        .unzip
        match { case (cartas, cantos) => (cartas.flatten, cantos.flatten) }
      )

    ManoDeBazasCompleta(jugadoresConLevantadasYCantos, ultimoLevantador, ultimoCanto, opcionesDePartido)
  }

}