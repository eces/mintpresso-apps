package controllers

trait TypeConversion {

  object Long {
    def unapply(s: String):Option[Long] = try {
      Some(s.toLong)
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }

}