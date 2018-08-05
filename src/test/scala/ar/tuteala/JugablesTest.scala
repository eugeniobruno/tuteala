package ar.tuteala

import org.junit.Test

class JugablesTest extends TuteTest {
  val helper = JugablesTestHelper

  @Test
  def `Las cartas jugables se determinan correctamente` =
    Mazo.palos.foreach { p =>
      testearIgualdades(helper(p).igualdadesParaTestear)
    }
}