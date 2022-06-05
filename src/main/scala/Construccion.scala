case class Construccion(construcciones: List[OrdenConstruccion]) //Map[DiaActual, ordenConstruccion]

object Construccion {
  def apply(construcciones: List[OrdenConstruccion]): Construccion = {
    new Construccion(construcciones)
  }
  def mostrarTodasConstrucciones(construcciones: Construccion): Unit ={
    construcciones.construcciones.foreach(construccion => {
      println("------ CONSTRUCCION ------")
      println("Tipo de construccion: " + construccion.solicitud.tipoConstruccion.nombre)
      println("Coordenada X: " + construccion.solicitud.coordenadaX)
      println("Coordenada Y: " + construccion.solicitud.coordenadaY)
      println("Estado Solicitud: " + construccion.estado)
      println("Dia Inicio: " + construccion.diaInicio)
      println("Dia Fin: " + construccion.diaFin)
    });
  }

  def mostrarFechaDeFinalizacion(construccion: Construccion): Unit ={
    val diaFinalizacion = construccion.construcciones.map(construccion => construccion.solicitud.tipoConstruccion.duracionDias).sum
    println("Se finalizará en: " + diaFinalizacion + " días")
  }

  def mostrarConstruccionesPendientes(construccion: Construccion): Unit ={
    construccion.construcciones.foreach(construccion => {
      if(construccion.estado.equalsIgnoreCase("PENDIENTE")){
        println("------ CONSTRUCCION PENDIENTE------")
        println("Tipo de construccion: " + construccion.solicitud.tipoConstruccion.nombre)
        println("Coordenada X: " + construccion.solicitud.coordenadaX)
        println("Coordenada Y: " + construccion.solicitud.coordenadaY)
        println("Estado Solicitud: " + construccion.estado)
        println("Dia Inicio: " + construccion.diaInicio)
        println("Dia Fin: " + construccion.diaFin)
      }
    });
  }

  def mostrarConstruccionesFinalizadas(construccion: Construccion): Unit ={
    construccion.construcciones.foreach(construccion => {
      if(construccion.estado.equalsIgnoreCase("FINALIZADO")){
        println("------ CONSTRUCCION FINALIZADO------")
        println("Tipo de construccion: " + construccion.solicitud.tipoConstruccion.nombre)
        println("Coordenada X: " + construccion.solicitud.coordenadaX)
        println("Coordenada Y: " + construccion.solicitud.coordenadaY)
        println("Estado Solicitud: " + construccion.estado)
        println("Dia Inicio: " + construccion.diaInicio)
        println("Dia Fin: " + construccion.diaFin)
      }
    });
  }

  def mostrarConstruccionesEnCurso(construccion: Construccion): Unit ={
    construccion.construcciones.foreach(construccion => {
      if(construccion.estado.equalsIgnoreCase("En proceso")){
        println("------ CONSTRUCCION EN PROCESO------")
        println("Tipo de construccion: " + construccion.solicitud.tipoConstruccion.nombre)
        println("Coordenada X: " + construccion.solicitud.coordenadaX)
        println("Coordenada Y: " + construccion.solicitud.coordenadaY)
        println("Estado Solicitud: " + construccion.estado)
        println("Dia Inicio: " + construccion.diaInicio)
        println("Dia Fin: " + construccion.diaFin)
      }
    });
  }


}
