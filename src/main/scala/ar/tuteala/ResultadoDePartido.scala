package ar.tuteala

case class ResultadoDePartido(jugadoresConPuntos: List[(Jugador, Int)]) {

  def esFinal(limiteDePuntos: Int): Boolean =
    jugadoresConPuntos exists { case (_, pts) => pts >= limiteDePuntos }

  // TODO chequear si va el getorelse. casi seguro que esta perfecto
  def puntosDe(jugador: Jugador): Int =
    jugadoresConPuntos.toMap.get(jugador).getOrElse(0)

  def sumadoCon(otroResultado: ResultadoDePartido, limiteDePuntos: Int): ResultadoDePartido = {
    val jugadoresConPuntosSumados = jugadoresConPuntos map {
      case (j, pts) => (j, (pts + otroResultado.puntosDe(j)) min limiteDePuntos)
    }
    ResultadoDePartido(jugadoresConPuntosSumados)
  }

  def ordenado: List[(Jugador,Int)] =
    jugadoresConPuntos sortBy (_._2)

  def perdedores(limiteDePuntos: Int): List[Jugador] =
    jugadoresConPuntos collect {
      case (j, pts) if pts >= limiteDePuntos => j
    }
}

object ResultadoDePartido {

  def suman(puntos: Int, perdedores: List[Jugador]): ResultadoDePartido =
    ResultadoDePartido( perdedores map ((_, puntos)) )

  def sumanUno(perdedores: List[Jugador]): ResultadoDePartido =
    suman(1, perdedores)
}