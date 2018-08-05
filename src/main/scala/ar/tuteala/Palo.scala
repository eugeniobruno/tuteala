package ar.tuteala

sealed trait Palo {
  def siguiente: Palo
}
final case object Oro extends Palo {
  val siguiente: Palo = Copa
}
final case object Copa extends Palo {
  val siguiente: Palo = Espada
}
final case object Espada extends Palo {
  val siguiente: Palo = Basto
}
final case object Basto extends Palo {
  val siguiente: Palo = Oro
}