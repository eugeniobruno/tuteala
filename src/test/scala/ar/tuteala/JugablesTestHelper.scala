package ar.tuteala

import ar.tuteala.TuteUtils._

case class JugablesTestHelper(triunfo: Palo) extends TuteTestHelper {

  val primerPalo = triunfo.siguiente
  val otroPalo = primerPalo.siguiente
  val numerosParaMano: List[Numero] = List(Cuatro, Seis, Diez, Tres)
  val numerosParaBaza: List[Numero] = List(Dos, Cinco, Siete, Cinco, Uno)

  val manoSin: List[Palo] => List[Carta] = ps =>
    for {
      p <- Mazo.palos sin ps
      n <- numerosParaMano
    } yield Carta(n, p)

  val manoConTodo = manoSin(Nil)
  val manoSinPrimerPalo = manoSin(List(primerPalo))
  val manoSinTriunfos = manoSin(List(triunfo))
  val manoSinPrimerPaloNiTriunfos = manoSin(List(primerPalo, triunfo))

  val manoSinPrimerPaloNiTriunfosAltos = manoSinPrimerPalo sin List(Carta(Seis, triunfo), Carta(Diez, triunfo), Carta(Tres, triunfo))

  val comoCarta: ((Numero, Palo)) => Carta = (Carta.apply _).tupled

  val comoJugada: Carta => (Jugador, Carta) = (null, _)

  val bazaCon: List[Carta] => Int => BazaParcial = { cs => n =>
    BazaParcial(cs take n map comoJugada, triunfo)
  }

  val cartasParaBaza: (Palo, Boolean) => List[Carta] = { (p, f) =>
    val segundoPalo = if (f) triunfo else p
    val palosParaBaza = List(p, segundoPalo, p, otroPalo, p)
    numerosParaBaza zip palosParaBaza map comoCarta
  }

  val bazaDeCon: (Palo, Boolean) => Int => BazaParcial = { (p, f) =>
    bazaCon(cartasParaBaza(p, f))
  }
  val bazaSinTriunfosCon: Int => BazaParcial = bazaDeCon(primerPalo, false)
  val bazaConTriunfosCon: Int => BazaParcial = bazaDeCon(primerPalo, true)
  val bazaConTriunfos2Con: Int => BazaParcial = bazaDeCon(triunfo, false)

  val bazaNueva = bazaSinTriunfosCon(0)


  val enManoDe: Palo => List[Carta] = { p => manoConTodo de p }

  val palosQueMatanLaEnesima: Palo => Int => List[Carta] = { p => n =>
    enManoDe(p) drop (n - 1)
  }

  // La primera carta de cada baza puede ser cualquiera
  def parteCero = List(
    (
      manoConTodo.toSet,
      manoConTodo.jugables(bazaNueva).toSet
    )
  )

  // Si no hay fallos con triunfos,
  // de todas las cartas en mesa del palo inicial, se debe matar la mas alta sin fallar
  // De no ser posible, se debe jugar una cualquiera del palo inicial
  def parteUno = {
    val esperados = List(1,2,3,3,1) map { n => palosQueMatanLaEnesima(primerPalo)(n).toSet }
    val obtenidos = 1 to 5 map { n => manoConTodo.jugables(bazaSinTriunfosCon(n)).toSet }
    esperados zip obtenidos
  }

  /*def parteUno = {
    val esperado = palosQueMatanLaEnesima(primerPalo)(3).toSet
    val obtenido = manoConTodo.jugables(bazaSinTriunfosCon(4)).toSet
    List((esperado, obtenido))
  }*/

  // Si hay fallos con triunfos, se debe jugar una cualquiera del palo inicial
  def parteDos = {
    val esperados = List(1,1,1,1,1) map { n => palosQueMatanLaEnesima(primerPalo)(n).toSet }
    val obtenidos = 1 to 5 map { n => manoConTodo.jugables(bazaConTriunfosCon(n)).toSet }
    esperados zip obtenidos
  }

  // Si el palo inicial es el triunfo, se aplican las reglas previas, de ser posible
  def parteTres = {
    val esperados = List(1,2,3,3,1) map { n => palosQueMatanLaEnesima(triunfo)(n).toSet }
    val obtenidos = 1 to 5 map { n => manoConTodo.jugables(bazaConTriunfos2Con(n)).toSet }
    esperados zip obtenidos
  }

  // Si no hay fallos con triunfos
  // y no se tienen cartas del palo inicial, se debe jugar cualquiera de triunfo
  def parteCuatro = {
    val esperados = List.fill(5)(enManoDe(triunfo).toSet)
    val obtenidos = 1 to 5 map { n => manoSinPrimerPalo.jugables(bazaSinTriunfosCon(n)).toSet }
    esperados zip obtenidos
  }

  // Si hay fallos con triunfos
  // y no se tienen cartas del palo inicial, se debe matar a la mas alta de triunfo
  def parteCinco = {
    val esperados = List(1,2,2,2,2) map { n => palosQueMatanLaEnesima(triunfo)(n).toSet }
    val obtenidos = 1 to 5 map { n => manoSinPrimerPalo.jugables(bazaConTriunfosCon(n)).toSet }
    esperados zip obtenidos
  }

  // Si el palo inicial es el triunfo, se aplican las reglas previas, de ser posible
  def parteSeis = {
    val esperados = List(1,2,3,3,1) map { n => palosQueMatanLaEnesima(triunfo)(n).toSet }
    val obtenidos = 1 to 5 map { n => manoSinPrimerPalo.jugables(bazaConTriunfos2Con(n)).toSet }
    esperados zip obtenidos
  }

  // Si no se tienen cartas del triunfo, se aplican las reglas previas, de ser posible
  def parteSiete = {
    val esperados = List(1,2,3,3,1) map { n => palosQueMatanLaEnesima(primerPalo)(n).toSet }
    val obtenidos = 1 to 5 map { n => manoSinTriunfos.jugables(bazaSinTriunfosCon(n)).toSet }
    esperados zip obtenidos
  }

  // Si no se tienen cartas del triunfo, se aplican las reglas previas, de ser posible
  def parteOcho = {
    val esperados = List(1,1,1,1,1) map { n => palosQueMatanLaEnesima(primerPalo)(n).toSet }
    val obtenidos = 1 to 5 map { n => manoSinTriunfos.jugables(bazaConTriunfosCon(n)).toSet }
    esperados zip obtenidos
  }

  // Si no se tienen cartas del triunfo ni del palo inicial, se puede jugar cualquiera
  def parteNueve = {
    val esperados = List.fill(5)(manoSinTriunfos.toSet)
    val obtenidos = 1 to 5 map { n => manoSinTriunfos.jugables(bazaConTriunfos2Con(n)).toSet }
    esperados zip obtenidos
  }

  // Si no se tienen cartas del triunfo ni del palo inicial, se puede jugar cualquiera
  def parteDiez = {
    val esperados = List.fill(5)(manoSinPrimerPaloNiTriunfos.toSet)
    val obtenidos = 1 to 5 map { n => manoSinPrimerPaloNiTriunfos.jugables(bazaSinTriunfosCon(n)).toSet }
    esperados zip obtenidos
  }

  // Si no se tienen cartas del triunfo ni del palo inicial, se puede jugar cualquiera
  def parteOnce = {
    val esperados = List.fill(5)(manoSinPrimerPaloNiTriunfos.toSet)
    val obtenidos = 1 to 5 map { n => manoSinPrimerPaloNiTriunfos.jugables(bazaConTriunfosCon(n)).toSet }
    esperados zip obtenidos
  }

  // Si no se tienen cartas del triunfo ni del palo inicial, se puede jugar cualquiera
  def parteDoce = {
    val esperados = List.fill(5)(manoSinPrimerPaloNiTriunfos.toSet)
    val obtenidos = 1 to 5 map { n => manoSinPrimerPaloNiTriunfos.jugables(bazaConTriunfos2Con(n)).toSet }
    esperados zip obtenidos
  }

  // Si no se tienen cartas del palo inicial, hubo un fallo con un triunfo
  // y no es posible matar al triunfo mas alto, se puede jugar cualquier carta
  def parteTrece = {
    val esperados = List(Set(Carta(Cuatro, triunfo))) ++ List.fill(4)(manoSinPrimerPaloNiTriunfosAltos.toSet)
    val obtenidos = 1 to 5 map { n => manoSinPrimerPaloNiTriunfosAltos.jugables(bazaConTriunfosCon(n)).toSet }
    esperados zip obtenidos
  }

  def igualdadesParaTestear: List[(Set[Carta], Set[Carta])] =
    List(
      parteCero,
      parteUno, parteDos, parteTres,
      parteCuatro, parteCinco, parteSeis,
      parteSiete, parteOcho, parteNueve,
      parteDiez, parteOnce, parteDoce,
      parteTrece
    ).flatten
}
