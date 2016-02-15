import scala.collection.mutable.Map
import scala.collection.immutable.{Map => IMap}

case class Warehouse(id: Int, position: (Int, Int), var products: Map[Int, Int]) {

  override def equals(o: Any) = o match {
    case that: Warehouse => that.id == id
    case _ => false
  }
  override def hashCode = id

  def getProductsThatAreInStockOutOf(ps: Map[Int, Int]): Map[Int, Int] = {
    val intersect = Map[Int, Int]()
    for ((productType, quantity) <- ps) {
      intersect(productType) = Math.min(quantity, products.getOrElse(productType, 0))
    }
    intersect
  }
  
  def removeProducts(ps: IMap[Int, Int]): Unit = {
    for ((productType, quantity) <- ps) {
      products(productType) -= quantity
    }
  }
  def removeProducts(ps: Map[Int, Int]): Unit = removeProducts(ps.toMap)

  def addProducts(ps: IMap[Int, Int]): Unit = {
    for ((productType, quantity) <- ps) {
      products(productType) = products.getOrElse(productType, 0) + quantity
    }
  }

  def distanceFrom(other: (Int, Int)) = {
    Simulation.instance.getDistance(position, other)
  }

}