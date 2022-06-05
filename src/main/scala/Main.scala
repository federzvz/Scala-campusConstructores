import Menu._

import scala.util.control._

object Main {

  def main(args: Array[String]): Unit = {
    val procesadorDias = ProcesadorDias.apply(1,false);
    val construcciones = Construccion.apply(List());
    procesarMenu(procesadorDias, construcciones);
  }


}