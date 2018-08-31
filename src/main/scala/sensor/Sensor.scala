package sensor

import java.util.Scanner

import scalafx.Includes._
import com.fazecast.jSerialComm.SerialPort
import scalafx.application.Platform
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
  private var _lowerRange: Seq[(Double,Double)] = Seq()
  private var _upperRange: Seq[(Double,Double)] = Seq()
  private var status =  "Connecting..." -> "black"

  def simStart(plot: LineChart[Number,Number], textFieldCurrentStateInfo: TextField, permittedRange: Array[Int] ): Unit = {
    port.setComPortTimeouts(SerialPort.TIMEOUT_SCANNER,0,0)
    port.openPort(1000)
    val thread = new Thread(){
      override def run(): Unit = {
        try {
          val scanner = new Scanner(port.getInputStream)
          while(scanner.hasNextLine) {
            updateValuesAndStatus(scanner,permittedRange)
            // FIXME update the plot
            //              plot.data <== ObservableBuffer(parseSequenceToPlottable("Sensor output", activeSensor.values))
            //              plot.getData.add(parseSequenceToPlottable("Sensor output", activeSensor.values))
            //              plot.getData.add(parseSequenceToPlottable(activeSensor.values.map({case(x,y) => (x,permittedRange(0).toDouble)})))
            //              plot.getData.add(parseSequenceToPlottable(activeSensor.values.map({case(x,y) => (x,permittedRange(1).toDouble)})))
            //              plot.data = Seq(parseSequenceToPlottable("Sensor output", activeSensor.values), parseSequenceToPlottable("put", activeSensor.values))
            //              plot.data = XYChart.Series[Number,Number]("Sensor output",ObservableBuffer(activeSensor.values.map{case (x,y) => XYChart.Data[Number,Number](x,y)}))
            Platform.runLater(plot.data_=(parseSequenceToPlottable("Sensor output", _values)))
            textFieldCurrentStateInfo.text = status._1
            Platform.runLater(textFieldCurrentStateInfo.setStyle(s"-fx-text-inner-color: ${status._2}; -fx-font-weight: bold"))
          }
          scanner.close()
        } catch {
          case ex: Exception => if(!port.isOpen) {
            println("Error = Port was unable to open!")
            textFieldCurrentStateInfo.text = "Connection error"
            textFieldCurrentStateInfo.setStyle(s"-fx-text-inner-color: red; -fx-font-weight: bold")
          } else {
            println("Error = Problem with scanning values from port!")
            textFieldCurrentStateInfo.text = "Scanning error"
            textFieldCurrentStateInfo.setStyle(s"-fx-text-inner-color: red; -fx-font-weight: bold")
          }
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
    textFieldCurrentStateInfo.setStyle("-fx-text-inner-color: black; -fx-font-weight: bold")
  }

  protected def updateValuesAndStatus(scanner: Scanner, permittedRange: Array[Int]): Unit = {
    val pattern = raw"${this.name} = (\d+.?\d+), Time = (\d+.?\d+)".r
    val scannedLine = scanner.nextLine()
    scannedLine match {
      case pattern(value,time) => try {
        val newSeq = Seq(math.rint(value.toDouble*100)/100 -> math.rint(time.toDouble*100)/100)
        _values = _values ++: newSeq
        _lowerRange = _lowerRange ++: Seq(permittedRange(0) -> newSeq._2)
        _upperRange = _upperRange ++: Seq(permittedRange(1) -> newSeq._2)
        if(newSeq.head._1 >= permittedRange(0) && newSeq.head._1 <= permittedRange(1)){
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

  def clearSequence(): Unit = {
    this._values = Seq()
    this._lowerRange = Seq()
    this._upperRange = Seq()
  }

  def toMap: Map[String,String] = {
    Map("name" -> name, "unit" -> unit, "minVal" -> minVal.toString, "maxVal" -> maxVal.toString)
  }

  protected def parseSequenceToPlottable(name: String, seq: Seq[(Double,Double)]): XYChart.Series[Number, Number] = {
    XYChart.Series[Number,Number](name, ObservableBuffer(seq.map{case (x,y) => XYChart.Data[Number,Number](x,y)}))
  }
}
