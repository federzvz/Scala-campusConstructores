import Construccion._
import ProcesadorDias.{siguienteJornada, validacionDiaria}

import scala.io.StdIn.readLine
import scala.util.control.Breaks

object Menu {
  def procesarMenu(jornada: ProcesadorDias, construcciones: Construccion): Unit = {
    val loop = new Breaks;
    val recursoCemento = Material.cantidadCementoInicial()
    val recursoGrava = Material.cantidadGravaInicial()
    val recursoArena = Material.cantidadArenaInicial()
    val recursoMadera = Material.cantidadMaderaInicial()
    val recursoAdobe = Material.cantidadAdobeInicial()
    val listaRecursos = List(recursoCemento, recursoGrava, recursoArena, recursoMadera, recursoAdobe)
    val construccionesValidadas =
    mostrarMenu(jornada)
    loop.breakable {
      Iterator.continually(io.StdIn.readLine.toString)
        .takeWhile(_ != "0")
        .foreach {
          case "1" => { //OPCIÓN: Siguiente Jornada(SE LLAMA A UNA FUNCIÓN RECURSIVA)
            procesarMenu(siguienteJornada(jornada), validacionDiaria(construcciones,jornada))
          }
          case "2" => { //OPCION: Solicitar Construcciones
            mostrarSubMenuSolicitarConstrucciones();
            loop.breakable {
              Iterator.continually(io.StdIn.readLine.toString).takeWhile(_ != "0").foreach {
                case "1" => { //SUBOPCION: Construir Casa
                  val tipoConstruccionCasa = TipoConstruccion.tipoCasa(listaRecursos);
                  val coordenadaX = readLine("Ingrese la coordenada X: ").toInt
                  val coordenadaY = readLine("Ingrese la coordenada Y: ").toInt
                  val solicitudConstruccion = Solicitud.apply(tipoConstruccionCasa, coordenadaX, coordenadaY)
                  solicitudConstruccion match {
                    case Left(stringMensajeNoHayMateriales) => {
                      println("---MENSAJE--- " + stringMensajeNoHayMateriales + " ---MENSAJE--- ")
                    }
                    case Right(solicitudCorrecta) => {
                      println("---MENSAJE--- Solicitud correcta ---MENSAJE---")
                      val ordenConstruccion = OrdenConstruccion.crearOrdenConstruccion(solicitudCorrecta, jornada, construcciones)
                      val construccionesActualizadas = Construccion.apply(List(ordenConstruccion) ++ construcciones.construcciones)
                      procesarMenu(jornada,construccionesActualizadas)
                    }
                  }
                  loop.break();
                }
                case "2" => { //SUBOPCION: Construir Lago
                  val tipoConstruccionLago = TipoConstruccion.tipoLago(listaRecursos);
                  val coordenadaX = readLine("Ingrese la coordenada X: ").toInt
                  val coordenadaY = readLine("Ingrese la coordenada Y: ").toInt
                  val solicitudConstruccion = Solicitud.apply(tipoConstruccionLago, coordenadaX, coordenadaY)
                  solicitudConstruccion match {
                    case Left(stringMensajeNoHayMateriales) => {
                      println("---MENSAJE--- " + stringMensajeNoHayMateriales + " ---MENSAJE--- ")
                    }
                    case Right(solicitudCorrecta) => {
                      println("---MENSAJE--- Solicitud correcta ---MENSAJE---")
                      val ordenConstruccion = OrdenConstruccion.crearOrdenConstruccion(solicitudCorrecta, jornada, construcciones)
                      val construccionesActualizadas = Construccion.apply(List(ordenConstruccion) ++ construcciones.construcciones)
                      procesarMenu(jornada,construccionesActualizadas)
                    }
                  }
                  loop.break();
                }
                case "3" => { //SUBOPCION: Construir Cancha de Futbol
                  val tipoConstruccionCancha = TipoConstruccion.tipoCanchaDeFutbol(listaRecursos);
                  val coordenadaX = readLine("Ingrese la coordenada X: ").toInt
                  val coordenadaY = readLine("Ingrese la coordenada Y: ").toInt
                  val solicitudConstruccion = Solicitud.apply(tipoConstruccionCancha, coordenadaX, coordenadaY)
                  solicitudConstruccion match {
                    case Left(stringMensajeNoHayMateriales) => {
                      println("---MENSAJE--- " + stringMensajeNoHayMateriales + " ---MENSAJE--- ")
                    }
                    case Right(solicitudCorrecta) => {
                      println("---MENSAJE--- Solicitud correcta ---MENSAJE---")
                      val ordenConstruccion = OrdenConstruccion.crearOrdenConstruccion(solicitudCorrecta, jornada, construcciones)
                      val construccionesActualizadas = Construccion.apply(List(ordenConstruccion) ++ construcciones.construcciones)
                      procesarMenu(jornada,construccionesActualizadas)
                    }
                  }
                  loop.break();
                }
                case "4" => { //SUBOPCION: Construir Edificio
                  val tipoConstruccionEdificio = TipoConstruccion.tipoEdificio(listaRecursos);
                  val coordenadaX = readLine("Ingrese la coordenada X: ").toInt
                  val coordenadaY = readLine("Ingrese la coordenada Y: ").toInt
                  val solicitudConstruccion = Solicitud.apply(tipoConstruccionEdificio, coordenadaX, coordenadaY)
                  solicitudConstruccion match {
                    case Left(stringMensajeNoHayMateriales) => {
                      println("---MENSAJE--- " + stringMensajeNoHayMateriales + " ---MENSAJE--- ")
                    }
                    case Right(solicitudCorrecta) => {
                      println("---MENSAJE--- Solicitud correcta ---MENSAJE---")
                      val ordenConstruccion = OrdenConstruccion.crearOrdenConstruccion(solicitudCorrecta, jornada, construcciones)
                      val construccionesActualizadas = Construccion.apply(List(ordenConstruccion) ++ construcciones.construcciones)
                      procesarMenu(jornada,construccionesActualizadas)
                    }
                  }
                  loop.break();
                }
                case "5" => { //SUBOPCION: Construir Gimnasio
                  val tipoConstruccionGimnasio = TipoConstruccion.tipoGimnasio(listaRecursos);
                  val coordenadaX = readLine("Ingrese la coordenada X: ").toInt
                  val coordenadaY = readLine("Ingrese la coordenada Y: ").toInt
                  val solicitudConstruccion = Solicitud.apply(tipoConstruccionGimnasio, coordenadaX, coordenadaY)
                  solicitudConstruccion match {
                    case Left(stringMensajeNoHayMateriales) => {
                      println("---MENSAJE--- " + stringMensajeNoHayMateriales + " ---MENSAJE--- ")
                    }
                    case Right(solicitudCorrecta) => {
                      println("---MENSAJE--- Solicitud correcta ---MENSAJE---")
                      val ordenConstruccion = OrdenConstruccion.crearOrdenConstruccion(solicitudCorrecta, jornada, construcciones)
                      val construccionesActualizadas = Construccion.apply(List(ordenConstruccion) ++ construcciones.construcciones)
                      procesarMenu(jornada,construccionesActualizadas)
                    }
                  }
                  loop.break();
                }
                case "0" => {
                  println("<-- Volver")
                }
              }
            }
            mostrarMenu(jornada);
          }
          case "3" => { //Mostrar Toda la información de cada construccion
            mostrarTodasConstrucciones(construcciones);
            mostrarMenu(jornada)
          }
          case "4" => { //Solicitar fecha de finalización de construcciones
            mostrarFechaDeFinalizacion(construcciones);
            mostrarMenu(jornada)
          }
          case "5" => { //Solicitar informe de construcciones pendientes.
            mostrarConstruccionesPendientes(construcciones);
            mostrarMenu(jornada)
          }
          case "6" => { //Solicitar informe de construcciones terminadas.
            mostrarConstruccionesFinalizadas(construcciones);
            mostrarMenu(jornada)
          }
          case "7" => { //Solicitar informe de construcciones en progreso.
            mostrarConstruccionesEnCurso(construcciones);
            mostrarMenu(jornada)
          }
          case _ => {
            println("Invalid option"); mostrarMenu(jornada)
          }
        }
    }
  }


  def mostrarMenu(diaActual: ProcesadorDias):Unit={
    mostrarJornadaActual(diaActual)
    println("1. Avanzar en el tiempo.")
    println("2. Solicitar Construccion.")
    println("3. Mostrar Construcciones.")
    println("4. Solicitar fecha de finalización.")
    println("5. Solicitar informe de construcciones pendientes.")
    println("6. Solicitar informe de construcciones terminadas.")
    println("7. Solicitar informe de construcciones en progreso.")
    println("0. Salir")
    print("OPCIÓN--> ")
  }

  def mostrarJornadaActual(diaActual: ProcesadorDias): Unit ={
    print("DIA:" + diaActual.diaActual.toString + " - Jornada: ")
    if(diaActual.night){
      println("Noche")
    }else{
      println("Dia")
    }
  }

  def mostrarSubMenuSolicitarConstrucciones(): Unit ={
    println("1. Construir casa")
    println("2. Construir lago")
    println("3. Construir Cancha de fútbol")
    println("4. Construir Edificio")
    println("5. Construir Gimnasio")
    println("0. Volver")
    print("OPCIÓN--> ")
  }
}
