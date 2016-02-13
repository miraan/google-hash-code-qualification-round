import scala.collection.mutable.Map
import scala.collection.mutable.Set

object Simulation {
  var instance: Simulation = _
}

class Simulation {
  var rows: Int = _
  var cols: Int = _
  var currentTime: Int = _
  var maxTime: Int = _
  var numberOfDrones: Int = _
  var droneCapacity: Int = _
  var numberOfProductTypes: Int = _
  var productTypeWeights: Array[Int] = _
  var numberOfWarehouses: Int = _

  var warehouses: List[Warehouse] = _
  var drones: List[Drone] = _
  var orders: List[Order] = _

  var totalScore = 0
  var completedOrders = 0
  def allOrdersComplete = orders.length == completedOrders

  var commands: List[Command] = List()

  def run(): Unit = {
    printDescriptionOfSimulationParameters()
    currentTime = 0
    while (currentTime < maxTime && !allOrdersComplete) doTurn()

    println("FINISHED at " + currentTime + " (max time " + maxTime + ")")
    println("TOTAL SCORE: " + totalScore)

    val output = commands.length + "\n" + commands.reverse.map(_.toString).mkString("\n")
    FileInput.writeOutput(output)
    println("wrote output")
  }

  def doTurn(): Unit = {
    for (drone <- drones) drone.updateState()
    for (drone <- drones; if !drone.isBusy) {
      sendDrone(drone)
    }

    currentTime += 1
    if (currentTime % 1000 == 0) println("current time: " + currentTime)
  }

  // products are represented as a Map[Int, Int] from product types to their respective quantities
  // we define a shipment as a subset of products that occur in one order, from one warehouse
  // and can fit on a given drone
  // sendDrone, below, will first find the optimal initial shipment to load onto the drone
  // then, will keep loading shipment, in optimal order, from the same warehouse as the initial one, onto the drone
  // while more can still fit

  def sendDrone(drone: Drone): Unit = {
    var os: List[(Order, Warehouse, Map[Int, Int])] = getInitialShipmentsListForDrone(drone)
    if (os.isEmpty) {
      println("all orders complete")
      return
    }
    val warehouse: Warehouse = os.head._2
    var orderShipments: List[(Order, Map[Int, Int])] = List()

    while (!os.isEmpty) {
      val order = os.head._1
      val shipment = os.head._3

      warehouse.removeProducts(shipment)
      drone.addProducts(shipment)
      order.removeProducts(shipment)

      orderShipments ::= (order, shipment)

      os = getSubsequentShipmentsListForDrone(drone, warehouse, order)
    }

    val allProductTypes = Set[Int]()
    for ((order, shipment) <- orderShipments) {
      for ((productType, quantity) <- shipment) allProductTypes + productType
    }
    val totalLoadTime = drone.distanceFrom(warehouse.position) + allProductTypes.size

    var totalDeliverTime = 0
    var previousOrder: Order = null
    for ((order, shipment) <- orderShipments.reverse) {
      if (previousOrder == null) {
        totalDeliverTime += warehouse.distanceFrom(order.position)
      } else {
        totalDeliverTime += previousOrder.distanceFrom(order.position)
      }
      val productTypesInShipment = Set[Int]()
      for ((productType, quantity) <- shipment) productTypesInShipment + productType
      totalDeliverTime += productTypesInShipment.size

      val timeOfDelivery = currentTime + totalLoadTime + totalDeliverTime - 1
      if (order.isComplete && timeOfDelivery < maxTime) {
        completedOrders += 1
        totalScore += scoreForOrderCompletedAt(timeOfDelivery)
        println("total score: " + totalScore)
      }

      previousOrder = order
    }

    for ((order, shipment) <- orderShipments.reverse) {
      for ((productType, quantity) <- shipment) {
        val load = Load(drone, warehouse, productType, quantity)
        commands ::= load
      }
    }
    for ((order, shipment) <- orderShipments.reverse) {
      for ((productType, quantity) <- shipment) {
        val deliver = Deliver(drone, order, productType, quantity)
        commands ::= deliver
      }
    }

    drone.isBusy = true
    drone.stopsBeingBusyAt = currentTime + totalLoadTime + totalDeliverTime
    drone.position = previousOrder.position
    drone.removeAllProducts()

    println("drone shipped for " + orderShipments.length + " orders")
  }

  def getInitialShipmentsListForDrone(drone: Drone): List[(Order, Warehouse, Map[Int, Int])] = {
    orders.filter(!_.isComplete)
    .flatMap((o: Order) => {
      for (w <- warehouses) yield (o, w)
    })
    .map((ow: (Order, Warehouse)) => {
      val s = getShipment(drone, ow._1, ow._2)
      (ow._1, ow._2, s)
    })
    .filter((ows: (Order, Warehouse, Map[Int, Int])) => {
      weightOfProducts(ows._3) > 0
    })
    .sortWith((ows1: (Order, Warehouse, Map[Int, Int]), ows2: (Order, Warehouse, Map[Int, Int])) => {
      getScoreForInitialShipment(drone, ows1._1, ows1._2, ows1._3) > getScoreForInitialShipment(drone, ows2._1, ows2._2, ows2._3)
    })
  }

  def getSubsequentShipmentsListForDrone(drone: Drone, warehouse: Warehouse, previousOrder: Order): List[(Order, Warehouse, Map[Int, Int])] = {
    orders.filter(!_.isComplete)
    .map((o: Order) => {
      val s = getShipment(drone, o, warehouse)
      (o, s)
    })
    .filter((os: (Order, Map[Int, Int])) => {
      weightOfProducts(os._2) > 0
    })
    .sortWith((os1: (Order, Map[Int, Int]), os2: (Order, Map[Int, Int])) => {
      getScoreForSubsequentShipment(drone, os1._1, os1._2, previousOrder) > getScoreForSubsequentShipment(drone, os2._1, os2._2, previousOrder)
    })
    .map((os: (Order, Map[Int, Int])) => {
      (os._1, warehouse, os._2)
    })
  }

  def getScoreForInitialShipment(drone: Drone, order: Order, warehouse: Warehouse, shipment: Map[Int, Int]): Double = {
    val p = order.percentageOfOrder(shipment)
    val d1 = drone.distanceFrom(warehouse.position)
    val d2 = warehouse.distanceFrom(order.position)
    val turns = d1 + d2 + (numberOfProductTypes(shipment) * 2)
    p / turns
  }
  
  def getScoreForSubsequentShipment(drone: Drone, order: Order, shipment: Map[Int, Int], previousOrder: Order): Double = {
    val d = previousOrder.distanceFrom(order.position)

    val m = numberOfProductTypes(shipment)

    val currentProductTypes = getProductTypes(drone.products)
    val newProductTypes = Set[Int]()
    for ((productType, quantity) <- shipment) if (!currentProductTypes.contains(productType)) newProductTypes + productType
    val n = newProductTypes.size

    val p = order.percentageOfOrder(shipment)
    val turns = d + m + n
    p / turns
  }

  var calculatedAverageDistance: Double = 0
  def averageDistance = {
    if (calculatedAverageDistance == 0) {
      val distances = orders.flatMap((o: Order) => {
        for (w <- warehouses) yield (o, w)
      })
        .map((ow: (Order, Warehouse)) => {
        getDistance(ow._1.position, ow._2.position)
      })
      val sum = distances.foldLeft(0)((total: Int, x: Int) => total + x)
      calculatedAverageDistance = sum.toDouble / distances.length
    }
    calculatedAverageDistance
  }

  def getShipment(drone: Drone, order: Order, warehouse: Warehouse): Map[Int, Int] = {
    var shipment = warehouse.getProductsThatAreInStockOutOf(order.products)
    shipment = drone.getProductsThatCanFitOutOf(shipment)
    shipment
  }

  def getDistance(a: (Int, Int), b: (Int, Int)): Int = {
    val dx2 = Math.abs(a._1 - b._1) * Math.abs(a._1 - b._1)
    val dy2 = Math.abs(a._2 - b._2) * Math.abs(a._2 - b._2)
    Math.sqrt(dx2 + dy2).ceil.toInt
  }

  def weightOfProducts(shipment: Map[Int, Int]) =
    shipment.foldLeft(0)((sum: Int, product: (Int, Int)) => { sum + (productTypeWeights(product._1) * product._2) })
  
  def numberOfProductTypes(shipment: Map[Int, Int]) =
    shipment.foldLeft(0)((count: Int, product: (Int, Int)) => { count + (if (product._2 > 0) 1 else 0) } )

  def getProductTypes(shipment: Map[Int, Int]): Set[Int] = {
    val productTypes = Set[Int]()
    for ((productType, quantity) <- shipment) productTypes + productType
    productTypes
  }

  def printCommands(): Unit = for (command <- commands) command.print

  def scoreForOrderCompletedAt(time: Int): Int = {
    (((maxTime - time).toDouble / maxTime) * 100).ceil.toInt
  }

  def printDescriptionOfSimulationParameters(): Unit = {
    println("Simulation Parameters: ")
    println("Rows: " + rows)
    println("Cols: " + cols)
    println("Max Time: " + maxTime)
    println("Drones: " + drones.size)
    println("Drone Capacity: " + droneCapacity)
    println("Product Types:" + productTypeWeights.length)
    for (i <- 0 until productTypeWeights.length) print(i + " (" + productTypeWeights(i) + "kg) ")
    println()
    println("Warehouses: " + warehouses.length)
    println("Orders: " + orders.length)
  }
}
