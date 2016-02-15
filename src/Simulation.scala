import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.immutable.{Map => IMap, Set => ISet}

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

  def sendDrone(drone: Drone): Unit = {
    drone.removeAllProducts()
    getOptimalShipmentList(drone) match {
      case None => {
        println("all orders complete")
        return
      }
      case Some(shipmentList) => {
        val analysis = analyseShipmentList(shipmentList)

        var loadCommands = List[Command]()
        var deliverCommands = List[Command]()

        for (shipment <- shipmentList) {
          val (lCommands, dCommands) = shipment.load()
          loadCommands :::= lCommands
          deliverCommands :::= dCommands
        }
        for (c <- loadCommands.reverse) commands ::= c
        for (c <- deliverCommands.reverse) commands ::= c

        drone.isBusy = true
        drone.stopsBeingBusyAt = currentTime + analysis._3
        drone.position = shipmentList.last.order.position
        drone.removeAllProducts()

        println("drone sent with " + shipmentList.length + " shipments, scaled score: " + analysis._1.ceil.toInt + ", actual score: " + analysis._2.ceil.toInt + ", total time: " + analysis._3)
        if (analysis._2 > 0) {
          completedOrders += analysis._4
          totalScore += analysis._2.ceil.toInt
          println("total score: " + totalScore)
          println("completed orders: " + completedOrders + "/" + orders.length)
        }
      }
    }
  }

  def getOptimalShipmentList(drone: Drone): Option[List[Shipment]] = {
    val branchingFactor = 3

    val initialList = getInitialShipmentsListForDrone(drone)
    if (initialList.isEmpty) return None

    var possibilities: List[List[Shipment]] = initialList.take(branchingFactor).map(List(_))
    var roots: List[List[Shipment]] = List()
    var loop = true
    while (loop) {
      loop = false
      possibilities = possibilities.flatMap((shipmentList) => {
        val nextList = getNextShipmentsList(shipmentList).take(branchingFactor)
        if (!nextList.isEmpty) {
          loop = true
          roots ::= shipmentList
          nextList.map(_ :: shipmentList)
        } else {
          List(shipmentList)
        }
      })
    }
    possibilities :::= roots

    val optimalShipmentList =
      possibilities
      .map(shipmentList => (shipmentList, analyseShipmentList(shipmentList.reverse)))
      .sortWith((a, b) => a._2._1 > b._2._1)
      .head._1
    println("considered " + possibilities.length + " possible shipment lists")
    Some(optimalShipmentList.reverse)
  }

  def getInitialShipmentsListForDrone(drone: Drone): List[Shipment] = {
    drone.removeAllProducts()
    orders.filter(!_.isComplete)
      .flatMap((o: Order) => {
      for (w <- warehouses) yield (o, w)
    })
      .map((ow: (Order, Warehouse)) => {
      Shipment(drone, ow._1, ow._2)
    })
      .filter(_.hasProducts)
      .map((shipment) => {
      (shipment, shipment.analyseAsInitialShipment())
    })
      .filter(_._2._3 + currentTime - 1 < maxTime)
      .sortWith((a, b) => {
      a._2._1 > b._2._1
    })
      .map(_._1)
  }

  def getNextShipmentsList(currentShipmentList: List[Shipment]): List[Shipment] = {
    val drone = currentShipmentList.head.drone
    drone.removeAllProducts()
    val warehouse = currentShipmentList.head.warehouse
    for (shipment <- currentShipmentList) {
      drone.addProducts(shipment.products)
      warehouse.removeProducts(shipment.products)
      shipment.order.removeProducts(shipment.products)
    }

    val nextShipmentsList = orders.filter(!_.isComplete)
      .map((order) => {
      Shipment(drone, order, warehouse)
    })
      .filter(_.hasProducts)
      .map((shipment) => (shipment, getScoreForLastShipment(shipment::currentShipmentList)))
      .sortWith((a, b) => a._2 > b._2)
      .map(_._1)

    drone.removeAllProducts()
    for (shipment <- currentShipmentList) {
      warehouse.addProducts(shipment.products)
      shipment.order.addProducts(shipment.products)
    }

    nextShipmentsList
  }

  def getScoreForLastShipment(shipmentList: List[Shipment]): Double = {
    val shipment = shipmentList.head
    val previousShipment = shipmentList.tail.head

    val d = previousShipment.order.distanceFrom(shipment.order.position)

    val m = shipment.numberOfProductTypes

    val currentProductTypes = getProductTypes(shipmentList.tail)
    val newProductTypes = Set[Int]()
    for ((productType, quantity) <- shipment.products) if (!currentProductTypes.contains(productType)) newProductTypes + productType
    val n = newProductTypes.size

    val p = shipment.percentageOfOrder
    val turns = d + m + n
    p / turns
  }

  // returns (scaled score, actual score, total time, number of completed orders)
  def analyseShipmentList(shipmentList: List[Shipment]): (Double, Double, Int, Int) = {
    val drone = shipmentList.head.drone
    val w = shipmentList.head.warehouse // assume warehouse is fixed for shipment list
    val d1 = drone.distanceFrom(w.position)
    val totalLoadTime = d1 + numberOfProductTypes(shipmentList)
    var totalTime = totalLoadTime
    var completedOrders = 0

    val firstShipment = shipmentList.head

    val deliverTime = w.distanceFrom(firstShipment.order.position) + firstShipment.numberOfProductTypes
    totalTime += deliverTime

    val p = firstShipment.percentageOfOrder
    val scaledScore = p * scoreForOrderCompletedAt(currentTime + totalTime - 1)
    var totalScaledScore = scaledScore
    val actualScore = if (p == 1) {
      completedOrders += 1
      scaledScore
    } else 0
    var totalActualScore = actualScore

    var previousShipment = firstShipment
    for (shipment <- shipmentList.tail) {
      val deliverTime = previousShipment.order.distanceFrom(shipment.order.position) + shipment.numberOfProductTypes
      totalTime += deliverTime

      val p = shipment.percentageOfOrder
      val scaledScore = p * scoreForOrderCompletedAt(currentTime + totalTime - 1)
      totalScaledScore += scaledScore
      val actualScore = if (p == 1) {
        completedOrders += 1
        scaledScore
      } else 0
      totalActualScore += actualScore

      previousShipment = shipment
    }

    totalScaledScore /= totalTime

    (totalScaledScore, totalActualScore, totalTime, completedOrders)
  }

  def getDistance(a: (Int, Int), b: (Int, Int)): Int = {
    val dx2 = Math.abs(a._1 - b._1) * Math.abs(a._1 - b._1)
    val dy2 = Math.abs(a._2 - b._2) * Math.abs(a._2 - b._2)
    Math.sqrt(dx2 + dy2).ceil.toInt
  }

  def weightOfProducts(ps: Map[Int, Int]) =
    ps.foldLeft(0)((sum: Int, product: (Int, Int)) => { sum + (productTypeWeights(product._1) * product._2) })
  def weightOfProducts(ps: collection.immutable.Map[Int, Int]) =
    ps.foldLeft(0)((sum: Int, product: (Int, Int)) => { sum + (productTypeWeights(product._1) * product._2) })

  def weightOfShipmentList(shipmentList: List[(Order, Warehouse, Map[Int, Int])]) =
    shipmentList.foldLeft(0)((sum: Int, shipment: (Order, Warehouse, Map[Int, Int])) => sum + weightOfProducts(shipment._3))

  def numberOfProductTypes(shipmentList: List[Shipment]) = {
    val productTypes = Set[Int]()
    for (shipment <- shipmentList) {
      for ((productType, quantity) <- shipment.products) productTypes + productType
    }
    productTypes.size
  }
  
  def numberOfProductTypes(ps: Map[Int, Int]) =
    ps.foldLeft(0)((count: Int, product: (Int, Int)) => { count + (if (product._2 > 0) 1 else 0) } )

  def getProductTypes(ps: Map[Int, Int]): Set[Int] = {
    val productTypes = Set[Int]()
    for ((productType, quantity) <- ps) productTypes + productType
    productTypes
  }

  def getProductTypes(shipmentList: List[Shipment]): Set[Int] = {
    val productTypes = Set[Int]()
    for (shipment <- shipmentList) {
      for ((productType, quantity) <- shipment.products) productTypes + productType
    }
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
