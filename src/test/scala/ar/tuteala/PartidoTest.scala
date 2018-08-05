package ar.tuteala

import java.util.Calendar

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.runner.RunWith

import ar.tuteala._

class PartidoTest {

  @Before
  def before = println("Vamos a correr un test")

  @Test
  def `partido de 3 jugadores` = {
    println("Inicio: " + Calendar.getInstance().getTime())
    val partidoCompleto = Partido.nuevo(PerdedoresTestHelper(3).jugadores, OpcionesDePartido(Nada, 10)).jugar
    println("Finnnn: " + Calendar.getInstance().getTime() )
    println(partidoCompleto.toString)
    println("")
  }

  @Test
  def `partido de 4 jugadores` = {
    println("Inicio: " + Calendar.getInstance().getTime())
    val partidoCompleto = Partido.nuevo(PerdedoresTestHelper(4).jugadores).jugar
    println("Finnnn: " + Calendar.getInstance().getTime() )
    println(partidoCompleto.toString)
    println("")
  }

  @Test
  def `partido de 5 jugadores` = {
    println("Inicio: " + Calendar.getInstance().getTime())
    val partidoCompleto = Partido.nuevo(PerdedoresTestHelper(5).jugadores).jugar
    println("Finnnn: " + Calendar.getInstance().getTime() )
    println(partidoCompleto.toString)
    println("")
  }


}