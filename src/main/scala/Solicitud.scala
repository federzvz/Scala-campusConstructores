case class Solicitud(tipoConstruccion: TipoConstruccion, coordenadaX: Int, coordenadaY: Int)

object Solicitud{
  def apply(tipoConstruccion: TipoConstruccion, coordenadaX: Int, coordenadaY: Int): Either[String, Solicitud] = {
    tipoConstruccion.materiales.foreach(material => if(material.cantidad <= 0) {
      return Left("No hay materiales suficientes para construir")
    })
    Right(new Solicitud(tipoConstruccion, coordenadaX, coordenadaY))
  }
}


