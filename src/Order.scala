import scala.collection.mutable.Map
import scala.collection.immutable.{Map => IMap}

case class Order(id: Int, position: (Int, Int), products: Map[Int, Int]) {

  override def equals(o: Any) = o match {
    case that: Warehouse => that.id == id
    case _ => false
  }
  override def hashCode = id

  def isComplete: Boolean = totalWeight() == 0
  def totalWeight(): Int = Simulation.instance.weightOfProducts(products)

  def removeProducts(ps: IMap[Int, Int]): Unit = {
    for ((productType, quantity) <- ps) {
      products(productType) -= quantity
    }
  }
  def removeProducts(ps: Map[Int, Int]): Unit = {
    removeProducts(ps.toMap)
  }

  def addProducts(ps: IMap[Int, Int]): Unit = {
    for ((productType, quantity) <- ps) {
      products(productType) = products.getOrElse(productType, 0) + quantity
    }
  }

  def percentageOfOrder(shipment: IMap[Int, Int]): Double =
    (Simulation.instance.weightOfProducts(shipment).toDouble / Simulation.instance.weightOfProducts(products))

  def percentageOfOrder(shipment: Map[Int, Int]): Double = percentageOfOrder(shipment.toMap)

  def distanceFrom(other: (Int, Int)) = {
    Simulation.instance.getDistance(position, other)
  }
}