package ar.tuteala

import org.junit.Test

class PerdedoresTest extends TuteTest {
  val helper = PerdedoresTestHelper

  @Test
  def `Los perdedores se determinan correctamente` =
    helper.cantidadesPosiblesDeJugadores.foreach { n =>
      testearIgualdades(helper(n).igualdadesParaTestear)
    }
}