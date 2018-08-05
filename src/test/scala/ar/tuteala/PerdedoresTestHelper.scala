package ar.tuteala

import ar.tuteala.TuteUtils._

case class PerdedoresTestHelper(cantidadDeJugadores: Int) extends TuteTestHelper {

  val tresJugadores = List(Jugador("Mati"), Jugador("Mar"), Jugador("Pablito"))
  val cuatroJugadores = tresJugadores :+ Jugador("Mery")
  val cincoJugadores = cuatroJugadores :+ Jugador("Euge")

  def jugadores: List[Jugador] =
    Map(
      3 -> tresJugadores,
      4 -> cuatroJugadores,
      5 -> cincoJugadores
    ).get(cantidadDeJugadores).getOPincha("No se puede jugar de a " + cantidadDeJugadores)

  def todos: Set[Jugador] =
    jugadores.toSet

  def todosExceptoElPrimeroOrdenados: List[Jugador] =
    jugadores.tail

  def todosExceptoElPrimero: Set[Jugador] =
    todosExceptoElPrimeroOrdenados.toSet

  def soloElSegundo: Set[Jugador] =
    (todosExceptoElPrimeroOrdenados take 1).toSet

  def soloElPrimero: Set[Jugador] =
    (jugadores take 1).toSet

  def elSegundoYElTercero: Set[Jugador] =
    (jugadores take 3).tail.toSet

  def todasLasQueSumanPuntos: List[Carta] =
    Mazo.ordenado filter (_.sumaPuntos)

  val eneCartasDeBase: Int => List[Carta] = {
    val numerosQueNoSuman = List(Dos, Cuatro, Cinco, Seis, Siete)// todo definir en contexto de test un numeroHelper
    val cartasQueNoSuman = numerosQueNoSuman map (Carta(_, Oro))

    cartasQueNoSuman take _
  }

  val cuatroJugadoresConPalos: List[(Jugador, Palo)] =
    cuatroJugadores zip Mazo.palos

  val cuatroJugadoresEmpatados: JugadoresConLevantadasYCantos =
    cuatroJugadoresConPalos map { case (j, p) => (j, (Mazo.todasLasDe(p), List(NoCanto))) }

  val tresJugadoresEmpatados: JugadoresConLevantadasYCantos =
    cuatroJugadoresEmpatados.init // Sobran todos los bastos

  val cincoJugadoresEmpatados: JugadoresConLevantadasYCantos =
    // Lo que importa es que el primero de la lista tenga el 3 de oro y que todos sumen lo mismo
    cincoJugadores map ((_, (List(Carta(Tres, Oro), Carta(Siete, Oro)), List(LasVeinteEn(Oro)))))

  val todosEmpatados: JugadoresConLevantadasYCantos =
    cantidadDeJugadores match {
      case 3 => tresJugadoresEmpatados
      case 4 => cuatroJugadoresEmpatados
      case 5 => cincoJugadoresEmpatados
    }

  // uno tiene 10 puntos menos, que son compensados por las 10 de ultima
  val todosCasiEmpatados: JugadoresConLevantadasYCantos =
    todosEmpatados match {
      case (j,(ls,cs))::resto => (j, (ls sin Carta(Tres, Oro), cs))::resto
      case _ => ??? // para evitar un warning innecesario
    }

  val cincoJugadoresCon: List[(List[Carta], List[Canto])] => JugadoresConLevantadasYCantos =
    cincoJugadores zip _

  val cincoJugadoresOrdenadosSinEmpates: JugadoresConLevantadasYCantos = {
    val cincoNumerosOrdenados = List(Uno, Tres, Doce, Once, Diez)
    val levantadasYCantosOrdenadoPorPuntos: List[(List[Carta], List[Canto])] = cincoNumerosOrdenados map (n => (Mazo.todosLos(n), List(NoCanto)))
    cincoJugadoresCon(levantadasYCantosOrdenadoPorPuntos)
  }

  val todosOrdenadosSinEmpates: JugadoresConLevantadasYCantos =
    cincoJugadoresOrdenadosSinEmpates take cantidadDeJugadores

  val cincoJugadoresConCapoteador: Boolean => JugadoresConLevantadasYCantos = { c =>
    val canto = if (c) LasCuarenta else NoCanto
    val levantadasYCantosConCapoteador = (Mazo.ordenado, List(canto)) :: List.fill(4)((Nil, List(NoCanto)))
    cincoJugadoresCon(levantadasYCantosConCapoteador)
  }

  val todosConCapoteador: Boolean => JugadoresConLevantadasYCantos = c =>
    cincoJugadoresConCapoteador(c) take cantidadDeJugadores

  val cincoJugadoresConUnoQueHizoBase: JugadoresConLevantadasYCantos = {
    val levantadasYCantosConUnoQueHizoBase = (todasLasQueSumanPuntos, List(NoCanto)) :: (eneCartasDeBase(5), List(NoCanto)) :: List.fill(3)((Nil, List(NoCanto)))
    cincoJugadoresCon(levantadasYCantosConUnoQueHizoBase)
  }

  val cincoJugadoresConDosQueHicieronBase: JugadoresConLevantadasYCantos = {
    val levantadasYCantosConDosQueHicieronBase = (todasLasQueSumanPuntos, List(NoCanto)) :: (eneCartasDeBase(5), List(NoCanto)) :: (eneCartasDeBase(5), List(NoCanto)) :: List.fill(2)((Nil, List(NoCanto)))
    cincoJugadoresCon(levantadasYCantosConDosQueHicieronBase)
  }

  val todosConUnoQueHizoBase: JugadoresConLevantadasYCantos =
    cincoJugadoresConUnoQueHizoBase take cantidadDeJugadores

  val todosConDosQueHicieronBase: JugadoresConLevantadasYCantos =
    cincoJugadoresConDosQueHicieronBase take cantidadDeJugadores

  val perdedoresSiElUltimoLevantadorEsElPrimero: JugadoresConLevantadasYCantos => List[Jugador] = ls =>
    DeterminadorDePerdedores(ls.toMap, ls.head._1).perdedores

  val perdedoresSiTodosEmpatan: Set[Jugador] =
    perdedoresSiElUltimoLevantadorEsElPrimero(todosCasiEmpatados).toSet

  val perdedoresSiElPrimeroGanaYElRestoEmpata: Set[Jugador] =
    perdedoresSiElUltimoLevantadorEsElPrimero(todosEmpatados).toSet

  val perdedoresSiNoHayEmpates: Set[Jugador] =
    perdedoresSiElUltimoLevantadorEsElPrimero(todosOrdenadosSinEmpates).toSet

  val perdedoresSiElPrimeroCapotea: Boolean => Set[Jugador] = c =>
    perdedoresSiElUltimoLevantadorEsElPrimero(todosConCapoteador(c)).toSet

  val perdedoresSiElPrimeroCapoteaSinCantos: Set[Jugador] =
    perdedoresSiElPrimeroCapotea(false).toSet

  val perdedoresSiElPrimeroCapoteaConCantos: Set[Jugador] =
    perdedoresSiElPrimeroCapotea(true).toSet

  val perdedoresSiUnoHizoBase: Set[Jugador] =
    perdedoresSiElUltimoLevantadorEsElPrimero(todosConUnoQueHizoBase).toSet

  val perdedoresSiDosHicieronBase: Set[Jugador] =
    perdedoresSiElUltimoLevantadorEsElPrimero(todosConDosQueHicieronBase).toSet


  def igualdadesParaTestear: List[(Set[Jugador], Set[Jugador])] =
    List(
      (
        todos,
        perdedoresSiTodosEmpatan
      ),
      (
        todosExceptoElPrimero,
        perdedoresSiElPrimeroGanaYElRestoEmpata
      ),
      (
        soloElSegundo,
        perdedoresSiNoHayEmpates
      ),
      (
        todosExceptoElPrimero,
        perdedoresSiElPrimeroCapoteaSinCantos
      ),
      (
        soloElPrimero,
        perdedoresSiElPrimeroCapoteaConCantos
      ),
      (
        soloElSegundo,
        perdedoresSiUnoHizoBase
      ),
      (
        elSegundoYElTercero,
        perdedoresSiDosHicieronBase
      )
    )

}

object PerdedoresTestHelper {
  val cantidadesPosiblesDeJugadores = List(3, 4, 5)
}