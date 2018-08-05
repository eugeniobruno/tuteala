package ar.tuteala

import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.runner.RunWith

abstract class TuteTest {
  val helper: AnyRef //TODO

  def testearIgualdades[A](pares: List[(A, A)]): Unit =
    pares.foreach { par =>
      assertEquals(par._1, par._2)
    }
}