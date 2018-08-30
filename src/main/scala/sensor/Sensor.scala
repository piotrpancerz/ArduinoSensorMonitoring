package sensor

import java.util.Scanner

import scalafx.Includes._
import com.fazecast.jSerialComm.SerialPort
import scalafx.scene.chart.XYChart._
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.{LineChart, XYChart}
import scalafx.scene.control.TextField
import scalafx.scene.paint.Color

import scala.collection.mutable

abstract class Sensor {
  val name: String
  val unit: String
  val minVal: Int
  val maxVal: Int
  val portName: String = try SerialPort.getCommPorts()(0).getSystemPortName catch {case _: Exception => "COM3"}
  val port: SerialPort = SerialPort.getCommPort(portName)
  private val plotTitle = s"$name - Time Graph"
  private val yAxisLabel = s"$name ($unit})"
  private var _values: Seq[(Double,Double)] = Seq()
  private var status =  "Test" -> "red"

  def values = _values

  def simStart(plot: LineChart[Number,Number], textFieldCurrentStateInfo: TextField, permittedRange: Array[Int] ): Unit = {
    port.setComPortTimeouts(SerialPort.TIMEOUT_SCANNER,0,0)
    port.openPort(1000)
    val thread = new Thread(){
      override def run(): Unit = {
        try {
          val scanner = new Scanner(port.getInputStream)
          while(scanner.hasNextLine) {
            updateValuesAndStatus(scanner,permittedRange)
            // FIXME delete or comment
//            println(_values)
            // TODO update the plot or make values observable

            // TODO add textField color background and text (perhaps a function to do so)
            textFieldCurrentStateInfo.text = status._1
            textFieldCurrentStateInfo.setStyle(s"-fx-text-inner-color: ${status._2}; -fx-font-weight: bold")
          }
          scanner.close()
        } catch {
          case ex: Exception => if(!port.isOpen) println("Error = Port was unable to open!") else println("Error = Problem with scanning values from port!")
        }
      }
    }
    thread.start()
  }

  def simStop(textFieldCurrentStateInfo: TextField): Unit = {
    try {
      port.closePort()
    } catch {
      case ex: Exception => if(port.isOpen) println("Error = Port was unable to close!") else println("Error = Exception while closing port!")
    }
    textFieldCurrentStateInfo.text = "Simulation paused"
    //FIXME Fix visible effect(no border)
    textFieldCurrentStateInfo.setStyle("-fx-text-inner-color: black; -fx-font-weight:bold")
  }

  protected def updateValuesAndStatus(scanner: Scanner, permittedRange: Array[Int]): Unit = {
    val pattern = raw"${this.name} = (\d+.?\d+), Time = (\d+.?\d+)".r
    val scannedLine = scanner.nextLine()
    println(pattern)
    println(scannedLine)
    scannedLine match {
      case pattern(value,time) => try {
        println("good")
        val newValue = Seq(math.rint(value.toDouble*100)/100 -> math.rint(time.toDouble*100)/100)
        _values ++: newValue
        if(newValue.head._1 >= permittedRange(0) && newValue.head._1 <= permittedRange(1)){
          status =  "Value in range" -> "green"
        } else {
          status =  "Value out of range" -> "red"
        }
      } catch {
        case ex: NumberFormatException => {
          println("Error = Unable to parse scanner line to doubles!")
        }
      }
      case _ =>
    }
  }

  def clearSequence(): Unit = this._values = Seq()

  def toMap: Map[String,String] = {
    Map("name" -> name, "unit" -> unit, "minVal" -> minVal.toString, "maxVal" -> maxVal.toString)
  }
}
