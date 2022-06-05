
case class ProcesadorDias(diaActual: Int=1, night: Boolean)

object ProcesadorDias {
  def apply(diaActual: Int, night: Boolean): ProcesadorDias = new ProcesadorDias(diaActual, night)
  def siguienteJornada(procesarDias: ProcesadorDias): ProcesadorDias = {
    if(!procesarDias.night){ //Si es la primera jornada, pasamos a la noche
      return ProcesadorDias.apply(procesarDias.diaActual,true)
    }
    return ProcesadorDias.apply(procesarDias.diaActual+1,false)
  }

  def validacionDiaria(construccion: Construccion, jornada: ProcesadorDias): Construccion ={
    if(!jornada.night){
      val construccionActualizada = construccion.construcciones.map(construccion => {
        construccion.diaInicio match {
          case jornada.diaActual => construccion.copy(estado = "En proceso")
          case _ => {
            if(construccion.diaInicio <= jornada.diaActual && construccion.diaFin > jornada.diaActual){
              construccion.copy(estado = "En proceso")
            }else{
              construccion
            }
          }
        }
      })
      Construccion.apply(construccionActualizada)
    }else{
      val construccionActualizada = construccion.construcciones.map(construccion => {
        construccion.diaFin match {
          case jornada.diaActual => construccion.copy(estado = "Finalizado")
          case _ => {
            if(construccion.diaFin<jornada.diaActual){
              construccion.copy(estado = "Finalizado")
            }else{
              construccion
            }
          }
        }
      })
      Construccion.apply(construccionActualizada)
    }

  }
}
