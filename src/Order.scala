import scala.collection.mutable.Map

case class Order(id: Int, position: (Int, Int), products: Map[Int, Int]) {

  def isComplete: Boolean = totalWeight() == 0
  def totalWeight(): Int = Simulation.instance.weightOfProducts(products)

  def removeProducts(ps: Map[Int, Int]): Unit = {
    for ((productType, quantity) <- ps) {
      products(productType) -= quantity
    }
  }

  def percentageOfOrder(shipment: Map[Int, Int]): Double =
    (Simulation.instance.weightOfProducts(shipment).toDouble / Simulation.instance.weightOfProducts(products))

  def distanceFrom(other: (Int, Int)) = {
    Simulation.instance.getDistance(position, other)
  }
}