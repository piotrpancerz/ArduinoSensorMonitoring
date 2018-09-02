package sensor

import java.util.Scanner

import scalafx.Includes._
import com.fazecast.jSerialComm.SerialPort
import scalafx.application.Platform
import scalafx.scene.chart.XYChart._
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.TextField

abstract class Sensor {
  val name: String
  val unit: String
  val minVal: Int
  val maxVal: Int
  val portName: String = try SerialPort.getCommPorts()(0).getSystemPortName catch {case _: Exception => "COM3"}
  val port: SerialPort = SerialPort.getCommPort(portName)
  private val plotTitle = s"$name - Time Graph"
  private val yAxisLabel = s"$name ($unit})"
  private var _values: Seq[Double] = Seq()
  private var _timestamps: Seq[Double] = Seq()
  private var status =  "Connecting..." -> "black"

  def simStart(xAxis: NumberAxis, yAxis: NumberAxis, plot: LineChart[Number,Number], textFieldCurrentStateInfo: TextField, permittedRange: Array[Int] ): Unit = {
    port.setComPortTimeouts(SerialPort.TIMEOUT_SCANNER,0,0)
    port.openPort(1000)
    val thread = new Thread(){
      override def run(): Unit = {
        try {
          val scanner = new Scanner(port.getInputStream)
          while(scanner.hasNextLine) {
            updateValuesAndStatus(scanner,permittedRange)
            Platform.runLater({
              plot.data_=(parseSequenceToPlottable("Sensor output", _timestamps.zip(_values)))
              if(_values.nonEmpty && _timestamps.nonEmpty){
                xAxis.lowerBound = math.floor(_timestamps.head)
                xAxis.upperBound = math.ceil(_timestamps.last)
                xAxis.tickUnit = 1
                yAxis.lowerBound = math.floor(_values.min-1)
                yAxis.upperBound = math.ceil(_values.max+1)
                yAxis.tickUnit = 1
              }
              textFieldCurrentStateInfo.text = status._1
              textFieldCurrentStateInfo.setStyle(s"-fx-text-inner-color: ${status._2}; -fx-font-weight: bold")
            })
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

  def simStop(plot: LineChart[Number,Number], textFieldCurrentStateInfo: TextField): Unit = {
    try {
      port.closePort()
    } catch {
      case ex: Exception => if(port.isOpen) println("Error = Port was unable to close!") else println("Error = Exception while closing port!")
    }
    textFieldCurrentStateInfo.text = "Simulation paused"
    textFieldCurrentStateInfo.setStyle("-fx-text-inner-color: black; -fx-font-weight: bold")
    clearSequence()
    plot.getData.clear()
  }

  protected def updateValuesAndStatus(scanner: Scanner, permittedRange: Array[Int]): Unit = {
    val pattern = raw"${this.name} = (\d+.?\d+), Time = (\d+.?\d+)".r
    val scannedLine = scanner.nextLine()
    scannedLine match {
      case pattern(value,time) => try {
        val newValue = math.rint(value.toDouble*100)/100
        val newTimestamp = math.rint(time.toDouble*100)/100

        if(_values.length >= 10){
          _values = _values.drop(1)
          _timestamps = _timestamps.drop(1)
        }
        _values = _values ++: Seq(newValue)
        _timestamps = _timestamps ++: Seq(newTimestamp)

        if(_values.head >= permittedRange(0) && _values.head <= permittedRange(1)){
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
    this._timestamps = Seq()
  }

  def toMap: Map[String,String] = {
    Map("name" -> name, "unit" -> unit, "minVal" -> minVal.toString, "maxVal" -> maxVal.toString)
  }

  protected def parseSequenceToPlottable(name: String, seq: Seq[(Double,Double)]): XYChart.Series[Number, Number] = {
    XYChart.Series[Number,Number](name, ObservableBuffer(seq.map{case (x,y) => XYChart.Data[Number,Number](x,y)}))
  }
}
