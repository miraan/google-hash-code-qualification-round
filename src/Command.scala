abstract class Command {
  def print: Unit
  def toString: String
}

case class Load(drone: Drone, warehouse: Warehouse, productType: Int, quantity: Int) extends Command {
  override def toString = drone.id + " L " + warehouse.id + " " + productType + " " + quantity
  override def print = println(toString)
}

case class Deliver(drone: Drone, order: Order, productType: Int, quantity: Int) extends Command {
  override def toString = drone.id + " D " + order.id + " " + productType + " " + quantity
  override def print = println(toString)
}