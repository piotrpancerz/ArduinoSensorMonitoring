package sensor

case class LiquidLevel() extends Sensor{
  val name = "Liquid Level"
  val unit = "mm"
  val minVal = 0
  val maxVal = 200
}
