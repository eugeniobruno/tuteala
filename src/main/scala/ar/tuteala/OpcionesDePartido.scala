package ar.tuteala

case class OpcionesDePartido(valorDeTute: ValorDeTute = ElRestoPierde, aCuanto: Int = 4)

sealed trait ValorDeTute

final case object ElRestoPierde extends ValorDeTute
final case class Capotes(cuantos: Int) extends ValorDeTute
final case object Nada extends ValorDeTute