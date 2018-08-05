package ar.tuteala

import ar.tuteala.TuteUtils._

case class DeterminadorDePerdedores(jugadoresConLevantadasYCantos: Map[Jugador, (List[Carta], List[Canto])], ultimoLevantador: Jugador) {

  val jugadores: List[Jugador] =
    jugadoresConLevantadasYCantos.keySet.toList

  def perdedores: List[Jugador] =
    capoteadorConCantos.map {
      case (c, cantos) if cantos.noHay => jugadores sin c
      case (c, _) => List(c) // si el capoteador canto, pierde el
    }
    .getOrElse(perdedoresPorPuntos)

  protected def capoteadorConCantos: Option[(Jugador, List[Canto])] =
    capoteador map (c => (c, cantosDe(c)))

  protected def capoteador: Option[Jugador] =
    jugadores find (hizoCapote _)

  protected def hizoCapote(jugador: Jugador): Boolean =
    levantadasPor(jugador).toSet == Mazo.ordenado.toSet

  protected def levantadasPor(jugador: Jugador): List[Carta] =
    levantadasYCantosDe(jugador)._1

  protected def cantosDe(jugador: Jugador): List[Canto] =
    levantadasYCantosDe(jugador)._2

  protected def levantadasYCantosDe(jugador: Jugador): (List[Carta], List[Canto]) =
    jugadoresConLevantadasYCantos.get(jugador).getOPincha(jugador + " no figura en el mapa de jugadores con levantadas y cantos")

  protected def perdedoresPorPuntos: List[Jugador] = {
    // si todos levantaron puntos, esto funciona aunque haya empates arriba, abajo o en el medio.

    val puntajeEs = (puntaje: Option[Int]) => ((jugadorConPuntos: (Jugador, Int)) =>
      Some(jugadorConPuntos._2) == puntaje)

    // Asumiendo que no hubo capote, los candidatos a perder
    // son los que hicieron base, sin importar sus puntos.
    // Siempre existe un puntaje maximo mayor que cero
    val puntajeMasAlto = jugadoresQueHicieronBaseConPuntosOrdenados.head._2
    // Siempre hay uno o mas jugadores con ese puntaje
    val primeros = jugadoresQueHicieronBaseConPuntosOrdenados takeWhile (_._2 == puntajeMasAlto) map (_._1)
    // puede que los primeros sean todos, es decir, que sinPrimerosConPuntosOrdenados sea Nil
    val sinPrimerosConPuntosOrdenados = jugadoresQueHicieronBaseConPuntosOrdenados dropWhile (_._2 == puntajeMasAlto)
    // entre quienes hicieron base, podria no existir un segundo puntaje mas alto
    val segundoPuntajeMasAlto = sinPrimerosConPuntosOrdenados.headOption map (_._2) // puede ser Nil
    // y por ende, puede o no existir un conjunto de segundos
    val segundos = sinPrimerosConPuntosOrdenados match {
      case Nil => None
      case sp => Some(sp takeWhile puntajeEs(segundoPuntajeMasAlto) map (_._1))
    }

    // Si hay segundos, son los perdedores. Si no, pierden los primeros
    segundos getOrElse primeros
  }

  protected val jugadoresQueHicieronBaseConPuntosOrdenados: List[(Jugador, Int)] =
    jugadoresQueHicieronBaseConPuntos sortBy (- _._2)

  protected def jugadoresQueHicieronBaseConPuntos: List[(Jugador, Int)] =
    jugadoresConLevantadasYCantos.toList collect {
      case (jugador, (levantadas, cantos)) if levantadas != Nil =>
        (jugador, levantadas.puntos + cantos.puntos + puntosDeUltima(jugador))
    }

  protected def puntosDeUltima(jugador: Jugador): Int =
    if (jugador == ultimoLevantador) 10 else 0
}