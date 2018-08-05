package ar.tuteala

import ar.tuteala.TuteUtils._

sealed trait Numero extends Ordered[Numero] {
  def puntos: Int
  def compare(otro: Numero): Int = Numero.poderDel(this) compare Numero.poderDel(otro)
}

sealed trait NumeroSinPuntos extends Numero {
  val puntos = 0
}

final case object Uno extends Numero {
  val puntos = 11
}

final case object Dos extends NumeroSinPuntos

final case object Tres extends Numero {
  val puntos = 10
}

final case object Cuatro extends NumeroSinPuntos
final case object Cinco  extends NumeroSinPuntos
final case object Seis   extends NumeroSinPuntos
final case object Siete  extends NumeroSinPuntos

final case object Diez extends Numero {
  val puntos = 2
}

final case object Once extends Numero {
  val puntos = 3
}

final case object Doce extends Numero {
  val puntos = 4
}

object Numero {
  val ordenadosPorPoder: List[Numero] = List(Uno, Tres, Doce, Once, Diez, Siete, Seis, Cinco, Cuatro, Dos)
  val poderes: Map[Numero, Int] = (ordenadosPorPoder zip (10 to 1 by -1)).toMap

  def poderDel(numero: Numero): Int = poderes.get(numero).getOPincha(numero.toString + " no es un Numero")
}
