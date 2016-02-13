import scala.collection.mutable.Map

case class Warehouse(id: Int, position: (Int, Int), var products: Map[Int, Int]) {

  def getProductsThatAreInStock(ps: Map[Int, Int]): Map[Int, Int] = {
    val intersect = Map[Int, Int]()
    for ((productType, quantity) <- ps) {
      intersect(productType) = Math.min(quantity, products.getOrElse(productType, 0))
    }
    intersect
  }
  
  def removeProducts(ps: scala.collection.mutable.Map[Int, Int]): Unit = {
    for ((productType, quantity) <- ps) {
      products(productType) -= quantity
    }
  }

  def distanceFrom(other: (Int, Int)) = {
    Simulation.instance.getDistance(position, other)
  }

}