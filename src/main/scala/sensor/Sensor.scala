package sensor

import java.util.Scanner

import scalafx.Includes._
import com.fazecast.jSerialComm.SerialPort
import scalafx.scene.chart.XYChart._
import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.{LineChart, XYChart}
import scalafx.scene.control.TextField

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
  // FIXME Change values to Buffer? (And then change updateSequence and clearSequence method)
  var values: Seq[(Double,Double)] = Seq()

  def simStart(plot: LineChart[Number,Number], textFieldCurrentStateInfo: TextField, permittedRange: Array[Int] ): Unit = {
    port.setComPortTimeouts(SerialPort.TIMEOUT_SCANNER,0,0)
    port.openPort(1000)
    val thread = new Thread(){
      override def run(): Unit = {
        try {
          val scanner = new Scanner(port.getInputStream)
          while(scanner.hasNextLine) {
            values = updateSequence(values,scanner)
            // FIXME delete or comment println
            println(values)
            // TODO update the plot or make values observable

            // TODO add textField color background and text (perhaps a function to do so)
            textFieldCurrentStateInfo.text = "Test"
          }
          scanner.close()
        } catch {
          case ex: Exception => if(!port.isOpen) println("Error = Port was unable to open!") else println("Error = Problem with scanning values from port!")
        }
      }
    }
    thread.start()
  }

  // FIXME Remove plot from arguments
  def simStop(plot: LineChart[Number,Number], textFieldCurrentStateInfo: TextField): Unit = {
    try {
      port.closePort()
    } catch {
      case ex: Exception => if(port.isOpen) println("Error = Port was unable to close!") else println("Error = Exception while closing port!")
    }
    // TODO add textField color background reset
    textFieldCurrentStateInfo.text = "Simulation paused"
  }

  // FIXME Possibly change method to work with Buffers instead of Sequences
  protected def updateSequence(seq: Seq[(Double,Double)], scanner: Scanner): Seq[(Double,Double)] = {
    val pattern = raw"${this.name} = (\d+.?\d+), Time = (\d+.?\d+)".r
    val scannedLine = scanner.nextLine()
    scannedLine match {
      case pattern(value,time) => try {
        seq ++: Seq(math.rint(value.toDouble*100)/100 -> math.rint(time.toDouble*100)/100)
      } catch {
        case ex: NumberFormatException => {
          println("Error = Unable to parse scanner line to doubles!")
          seq
        }
      }
      case _ => seq
    }
  }

  // FIXME Possibly change method to work with Buffers instead of Sequences
  def clearSequence(): Unit = this.values = Seq()

  def toMap: Map[String,String] = {
    Map("name" -> name, "unit" -> unit, "minVal" -> minVal.toString, "maxVal" -> maxVal.toString)
  }
}
