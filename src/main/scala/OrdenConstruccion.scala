case class OrdenConstruccion(solicitud: Solicitud, estado: String, diaInicio: Int, diaFin: Int)

object OrdenConstruccion {
  def apply(solicitud: Solicitud, estado: String, diaInicio: Int, diaFin: Int): OrdenConstruccion = {
    new OrdenConstruccion(solicitud, estado, diaInicio, diaFin)
  }

  def crearOrdenConstruccion(solicitudCorrecta: Solicitud, jornada: ProcesadorDias, construcciones: Construccion): OrdenConstruccion = {
    OrdenConstruccion.apply(
      solicitudCorrecta,
      "Pendiente",
      if(construcciones.construcciones.isEmpty){
        jornada.diaActual
      }else{construcciones.construcciones.last.diaFin},
      if(construcciones.construcciones.isEmpty){
        jornada.diaActual+solicitudCorrecta.tipoConstruccion.duracionDias
      }else{
        construcciones.construcciones.last.diaFin+solicitudCorrecta.tipoConstruccion.duracionDias
      }
    )
  }
}
