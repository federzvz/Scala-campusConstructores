case class Material(id: String, nombre: String, cantidad: Int)

object Material {
  def apply(id: String, nombre: String, cantidad: Int): Material = new Material(id, nombre, cantidad)

  def cantidadCementoInicial(): Material ={
    val cemento = Material.apply("Ce","Cemento", 1000)
    cemento
  }

  def cantidadGravaInicial(): Material ={
    val grava = Material.apply("Gr","Grava", 1000)
    grava
  }

  def cantidadArenaInicial(): Material ={
    val arena = Material.apply("Ar","Arena", 1000)
    arena
  }

  def cantidadMaderaInicial(): Material ={
    val madera = Material.apply("Ma","Madera", 1000)
    madera
  }

  def cantidadAdobeInicial(): Material ={
    val adobe = Material.apply("Ad","Adobe", 1000)
    adobe
  }
}

