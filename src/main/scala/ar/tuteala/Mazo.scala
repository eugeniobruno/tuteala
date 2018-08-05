package ar.tuteala

import scala.util.Random

object Mazo {
  val palos: List[Palo] = List(Oro, Copa, Espada, Basto)
  val numeros: List[Numero] = List(Uno, Dos, Tres, Cuatro, Cinco, Seis, Siete, Diez, Once, Doce)
  val cantos: List[Canto] = (palos map (LasVeinteEn(_))) ++ List(LasCuarenta, Tute, NoCanto)

  val ordenado: List[Carta] =
    for {
      p <- palos
      n <- numeros
    } yield Carta(n, p)

  def mezclado: List[Carta] =
    Random.shuffle(this.ordenado)

  val todosLos: Numero => List[Carta] = n =>
    for (p <- palos) yield Carta(n, p)
    
  val todasLasDe: Palo => List[Carta] = p =>
    for (n <- numeros) yield Carta(n, p)
}