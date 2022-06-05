import Material._

case class TipoConstruccion(nombre: String, materiales: List[Material], duracionDias: Int)

object TipoConstruccion{
  def apply(
             nombre: String,
             materiales: List[Material],
             duracionDias: Int
           ): TipoConstruccion = new TipoConstruccion(nombre, materiales, duracionDias)

  def tipoCasa(listaRecursos: List[Material]): TipoConstruccion ={
    val listaMateriales = listaRecursos.map(recurso => {
      recurso.nombre.toLowerCase() match {
        case "cemento" => recurso.copy(cantidad = recurso.cantidad - 100)
        case "grava" => recurso.copy(cantidad = recurso.cantidad - 50)
        case "arena" => recurso.copy(cantidad = recurso.cantidad - 90)
        case "madera" => recurso.copy(cantidad = recurso.cantidad - 20)
        case "adobe" => recurso.copy(cantidad = recurso.cantidad - 100)
      }
    })
    val nombre = "Casa"
    val duracion = 3
    TipoConstruccion.apply(nombre, listaMateriales, duracion)
  }

  def tipoLago(listaRecursos: List[Material]): TipoConstruccion ={
    val listaMateriales = listaRecursos.map(recurso => {
      recurso.nombre.toLowerCase() match {
        case "cemento" => recurso.copy(cantidad = recurso.cantidad - 50)
        case "grava" => recurso.copy(cantidad = recurso.cantidad - 60)
        case "arena" => recurso.copy(cantidad = recurso.cantidad - 80)
        case "madera" => recurso.copy(cantidad = recurso.cantidad - 10)
        case "adobe" => recurso.copy(cantidad = recurso.cantidad - 20)
      }
    })
    val nombre = "Lago"
    val duracion = 2
    TipoConstruccion.apply(nombre, listaMateriales, duracion)
  }

  def tipoCanchaDeFutbol(listaRecursos: List[Material]): TipoConstruccion ={
    val listaMateriales = listaRecursos.map(recurso => {
      recurso.nombre.toLowerCase() match {
        case "cemento" => recurso.copy(cantidad = recurso.cantidad - 20)
        case "grava" => recurso.copy(cantidad = recurso.cantidad - 20)
        case "arena" => recurso.copy(cantidad = recurso.cantidad - 20)
        case "madera" => recurso.copy(cantidad = recurso.cantidad - 20)
        case "adobe" => recurso.copy(cantidad = recurso.cantidad - 20)
      }
    })
    val nombre = "Cancha De Futbol"
    val duracion = 1
    TipoConstruccion.apply(nombre, listaMateriales, duracion)
  }

  def tipoEdificio(listaRecursos: List[Material]): TipoConstruccion ={
    val listaMateriales = listaRecursos.map(recurso => {
      recurso.nombre.toLowerCase() match {
        case "cemento" => recurso.copy(cantidad = recurso.cantidad - 200)
        case "grava" => recurso.copy(cantidad = recurso.cantidad - 100)
        case "arena" => recurso.copy(cantidad = recurso.cantidad - 180)
        case "madera" => recurso.copy(cantidad = recurso.cantidad - 40)
        case "adobe" => recurso.copy(cantidad = recurso.cantidad - 200)
      }
    })
    val nombre = "Edificio"
    val duracion = 6
    TipoConstruccion.apply(nombre, listaMateriales, duracion)
  }

  def tipoGimnasio(listaRecursos: List[Material]): TipoConstruccion ={
    val listaMateriales = listaRecursos.map(recurso => {
      recurso.nombre.toLowerCase() match {
        case "cemento" => recurso.copy(cantidad = recurso.cantidad - 50)
        case "grava" => recurso.copy(cantidad = recurso.cantidad - 25)
        case "arena" => recurso.copy(cantidad = recurso.cantidad - 45)
        case "madera" => recurso.copy(cantidad = recurso.cantidad - 10)
        case "adobe" => recurso.copy(cantidad = recurso.cantidad - 50)
      }
    })
    val nombre = "Gimnasio"
    val duracion = 2
    TipoConstruccion.apply(nombre, listaMateriales, duracion)
  }
}
