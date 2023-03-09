package me.sgrouples.rogue

import java.util.Locale

private[rogue] object SupportedLocales {
  lazy val map: Map[String, Locale] = {
    val mb = Map.newBuilder[String, Locale]
    Locale.getAvailableLocales.foreach { l =>
      val key = l.toString
      if (key.nonEmpty) mb += (key -> l)
    }
    mb += ("nb" -> new Locale(
      "nb"
    )) // norwegian bokmål, missing from getAvailableLocales

    // supporting legacy ISO language codes on Java 17:
    mb += ("iw" -> new Locale("iw"))
    mb += ("ji" -> new Locale("ji"))
    mb += ("in" -> new Locale("in"))

    mb.result()
  }
}
