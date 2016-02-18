case class Shipment(drone: Drone, order: Order, warehouse: Warehouse, products: Map[Int, Int]) {
  def this(drone: Drone, order: Order, warehouse: Warehouse) =
    this(drone, order, warehouse, drone.getProductsThatCanFitOutOf(warehouse.getProductsThatAreInStockOutOf(order.products)).toMap)

  def hasProducts = Simulation.instance.weightOfProducts(products) > 0

  def numberOfProductTypes =
    products.foldLeft(0)((count: Int, product: (Int, Int)) => { count + (if (product._2 > 0) 1 else 0) } )

  def percentageOfOrder = order.percentageOfOrder(products)

  def load(): (List[Command], List[Command]) = {
    warehouse.removeProducts(products)
    drone.addProducts(products)
    order.removeProducts(products)

    var loadCommands = List[Command]()
    var deliverCommands = List[Command]()
    for ((productType, quantity) <- products) {
      val load = Load(drone, warehouse, productType, quantity)
      val deliver = Deliver(drone, order, productType, quantity)
      loadCommands ::= load
      deliverCommands ::= deliver
    }

    (loadCommands, deliverCommands)
  }

  def averageDistanceToWarehouse = {
    val numberOfWarehousesToAverage = 3
    val closest = Simulation.instance.warehouses
      .map(_.distanceFrom(order.position))
      .sortWith(_ < _)
      .take(numberOfWarehousesToAverage)
    val sum = closest.foldLeft(0)(_ + _)
    sum.toDouble / closest.length
  }

  def analyseAsInitialShipment(): (Double, Double, Int) = {
    val d1 = drone.distanceFrom(warehouse.position)
    val d2 = warehouse.distanceFrom(order.position)
    val turns = d1 + d2 + (numberOfProductTypes * 2)
    val p = percentageOfOrder
    val scaledScore = p / turns
    val actualScore = if (p == 1) Simulation.instance.scoreForOrderCompletedAt(Simulation.instance.currentTime + turns - 1) else 0
    (scaledScore, actualScore, turns)
  }
}

object Shipment {
  def apply(drone: Drone, order: Order, warehouse: Warehouse) = new Shipment(drone: Drone, order: Order, warehouse: Warehouse)
}