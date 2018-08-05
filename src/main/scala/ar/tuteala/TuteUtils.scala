package ar.tuteala

object TuteUtils {

  type JugadoresConCartas = List[(Jugador, List[Carta])]
  type JugadoresConLevantadasYCantos = List[(Jugador, (List[Carta], List[Canto]))]

  implicit class OptionEnriquecida[A](val option: Option[A]) {

    def getOPincha(mensaje: String): A =
      option getOrElse (throw new RuntimeException(mensaje))
  }

  implicit class ListaEnriquecida[A](lista: List[A]) {

    def sin(elemento: A): List[A] =
      lista filterNot (_ == elemento)

    def sin(sublista: List[A]): List[A] =
      lista filterNot (sublista.toSet)

    def elPrimeroSiendo(elemento: A, validarPertenencia: Boolean = true): List[A] = {
      if (validarPertenencia)
        require(lista contains elemento, "elemento debe estar en lista")
      lista match {
        case x::xs if x == elemento => lista
        case _ => lista.rotadaUnaPosicion.elPrimeroSiendo(elemento, false)
      }
    }

    def rotadaUnaPosicion: List[A] =
      lista match {
        case x::xs => xs :+ x
        case Nil => Nil
      }
  }

  implicit class ListaDeCartas(cartas: List[Carta]) {

    def tiene(subconjunto: List[Carta]): Boolean =
      subconjunto forall (cartas.toSet)

    def tiene(palo: Palo): Boolean =
      cartas exists { case Carta(_, p) => p == palo }

    def de(palo: Palo): List[Carta] =
      cartas filter { case Carta(_, p) => p == palo }

    def laQueMataTodasLasDemas(primerPalo: Option[Palo], triunfo: Palo): Option[Carta] =
      cartas.find (c => c.mata(cartas sin c, primerPalo, triunfo))

    /** Determina la legalidad de cada jugada segun las reglas del tute estilo Barrancas.
    *
    * @param carta la carta en esta lista que se quiere saber si es jugable en la `baza`
    * @param baza conoce las cartas jugadas desde la ultima vez que alguien levanto
    *
    * @return `true` si la `carta` es jugable en el contexto especificado, `false` otherwise.
    */
    def jugables(baza: BazaParcial): List[Carta] = {

      val lasDelPrimerPalo = baza.primerPalo match {
        case None => cartas
        case Some(palo) => cartas de palo
      }
      val lasQueMatan = cartas filter (baza.seriaLevantadora(_))
      val lasDelPrimerPaloQueMatan = lasDelPrimerPalo intersect lasQueMatan

      List(lasDelPrimerPaloQueMatan, lasDelPrimerPalo, lasQueMatan) find (!_.isEmpty) getOrElse cartas
    }

    def puntos: Int = (cartas map (_.puntos)).sum

    def tieneOnceYDoceDe(palo: Palo): Boolean =
      cartas tiene (List(Carta(Once, palo), Carta(Doce, palo)))

    def tieneTodosLos(numero: Numero): Boolean =
      cartas tiene Mazo.todosLos(numero)
  }

  implicit class SerieDeCantos(val cantos: List[Canto]) {

    def puntos: Int =
      (cantos map (_.puntos)).sum

    def noHay: Boolean =
      (cantos sin NoCanto).isEmpty
  }

  implicit class TernaEnriquecida[A,B,C](val terna: (A, B, C)) {
    def sin_1: (B, C) = terna match {
      case (_, y, z) => (y, z)
    }

  }
}