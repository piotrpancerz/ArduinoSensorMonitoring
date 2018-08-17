package sensor

case class Temperature() extends Sensor {
  val name = "Temperature"
  val unit = "\u2103"
  val minVal = -50
  val maxVal = 150
}
