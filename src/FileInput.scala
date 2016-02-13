import java.io.PrintWriter

import scala.io.Source
import scala.collection.mutable.Map

object FileInput {
  val dataset = "redundancy"

  def getFileLines(): Array[String] = {
    val filename = "/Users/tripr/Documents/Oxford/Google Hash Code Online Qualification Round/src/"+dataset+".in"
    val fileContents = Source.fromFile(filename).getLines()
    fileContents.toArray
  }

  def writeOutput(contents: String): Unit = {
    val filename = "/Users/tripr/Documents/Oxford/Google Hash Code Online Qualification Round/src/"+dataset+".out"
    new PrintWriter(filename) { write(contents); close }
  }

  def parseInput() = {
    val simulation = new Simulation
    val lines = getFileLines()

    val firstLine = lines(0).split("\\s+")
    val rows = firstLine(0).toInt
    simulation.rows = rows

    val cols = firstLine(1).toInt
    simulation.cols = cols

    val numberOfDrones = firstLine(2).toInt

    val turns = firstLine(3).toInt
    simulation.maxTime = turns

    val maxPayload = firstLine(4).toInt
    simulation.droneCapacity = maxPayload

    val numberOfProductTypes = lines(1).toInt
    simulation.numberOfProductTypes = numberOfProductTypes

    val productTypeWeights = lines(2).split("\\s+").map(_.toInt)
    simulation.productTypeWeights = productTypeWeights

    val numberOfWarehouses = lines(3).toInt
    var warehouses: List[Warehouse] = List()
    for (i <- 0 until numberOfWarehouses) {
      val warehousePosition = lines(4 + (i * 2)).split("\\s+").map(_.toInt)
      val warehouseStock = lines(5 + (i * 2)).split("\\s+").map(_.toInt)

      val products = Map[Int, Int]()
      for (productType <- 0 until warehouseStock.length) {
        for (i <- 0 until warehouseStock.length) {
          products(i) = warehouseStock(i)
        }
      }

      val position = (warehousePosition(0), warehousePosition(1))
      val warehouse = Warehouse(i, position, products)

      warehouses ::= warehouse
    }
    simulation.warehouses = warehouses

    simulation.numberOfDrones = numberOfDrones
    var drones: List[Drone] = List()
    val warehouse0Position = simulation.warehouses(0).position
    for (i <- 0 until numberOfDrones) {
      val drone = Drone(i, warehouse0Position, Map[Int, Int]())
      drones ::= drone
    }
    simulation.drones = drones

    var orders = List[Order]()
    val ordersLine = 4 + (numberOfWarehouses * 2)
    val numberOfOrders = lines(ordersLine).toInt
    for (i <- 0 until numberOfOrders) {
      val orderPosition = lines(ordersLine + 1 + (i * 3)).split("\\s+").map(_.toInt)
      val orderProductTypes = lines(ordersLine + 3 + (i * 3)).split("\\s+").map(_.toInt)

      val position = (orderPosition(0), orderPosition(1))
      val productTypes = scala.collection.mutable.Map[Int,Int]()
      for (productType <- orderProductTypes) {
        productTypes(productType) = productTypes.getOrElse(productType, 0) + 1
      }
      val order = Order(i, position, productTypes)
      orders ::= order
    }
    simulation.orders = orders

    Simulation.instance = simulation
  }

  def main(args: Array[String]) {
    parseInput()
    Simulation.instance.run()
  }
}
