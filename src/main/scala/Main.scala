import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control._
import scalafx.scene.layout.Region
import sensor._

object Main extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title = "Sensor Monitoring System"
    scene = new Scene(1200,500){

      /* Initialize sensors */
      val sensors = Vector(Temperature(),LiquidLevel())
      val sensorMaps: Vector[Map[String,String]] = sensors.map(_.toMap)

      /* Create GUI elements */
      val button = new Button()
      val comboBox = new ComboBox(sensorMaps.map(_("name")))
      val xAxis = new NumberAxis()
      val yAxis = new NumberAxis()
      val plot = new LineChart(xAxis,yAxis)
      val sliderLower = new Slider()
      val sliderUpper = new Slider()
      val textFieldLowerInfo = new TextField()
      val textFieldUpperInfo = new TextField()
      val textFieldCurrentStateInfo = new TextField()
      val textFieldLowerValue= new TextField()
      val textFieldUpperValue = new TextField()

      /* Define position of the GUI elements */
      setPosition(button,1000,350)
      setPosition(comboBox,850,100)
      setPosition(plot,50,50)
      setPosition(sliderLower,725,225)
      setPosition(sliderUpper,975,225)
      setPosition(textFieldLowerInfo,700,175)
      setPosition(textFieldUpperInfo,950,175)
      setPosition(textFieldCurrentStateInfo,750,350)
      setPosition(textFieldLowerValue,700,275)
      setPosition(textFieldUpperValue,950,275)

      /* Define size of the elements */
      plot.prefHeight = 400
      plot.prefWidth = 600
      textFieldLowerValue.maxHeight(30)
      textFieldUpperValue.maxHeight(30)


      /* Define initial state of the GUI elements */
      setUneditable(textFieldLowerInfo,textFieldUpperInfo,textFieldCurrentStateInfo,textFieldLowerValue,textFieldUpperValue)
      button.text = "Start"
      comboBox.value = sensors.head.name
      xAxis.label = "Time (s)"
      xAxis.lowerBound = 0
      xAxis.upperBound = 1
      yAxis.label = s"${sensors.head.name} (${sensors.head.unit})"
      yAxis.lowerBound = 0
      yAxis.upperBound = 100
      plot.title = s"${sensors.head.name} - Time Graph"
      textFieldLowerInfo.text = "Set lower limit"
      textFieldUpperInfo.text = "Set upper limit"
      textFieldCurrentStateInfo.text = "Press start"
      resetSliders(sliderLower, sliderUpper, textFieldLowerValue, textFieldUpperValue, comboBox.value.value.toString, sensors)

      /* Define actions performed by the GUI elements */
      button.onAction = (ae: ActionEvent) => {
        val activeSensor = getActiveSensor(comboBox.value.value,sensors).get
        button.text.value match {
          case "Start" =>
            // TODO Make values observable and update the plot or try to update the plot with different approach
            //              plot.data <== ObservableBuffer(parseSequenceToPlottable("Sensor output", activeSensor.values))
            //              plot.getData.add(parseSequenceToPlottable("Sensor output", activeSensor.values))
            //              plot.getData.add(parseSequenceToPlottable(activeSensor.values.map({case(x,y) => (x,permittedRange(0).toDouble)})))
            //              plot.getData.add(parseSequenceToPlottable(activeSensor.values.map({case(x,y) => (x,permittedRange(1).toDouble)})))
            //              plot.data = Seq(parseSequenceToPlottable("Sensor output", activeSensor.values), parseSequenceToPlottable("put", activeSensor.values))
            //              plot.data = XYChart.Series[Number,Number]("Sensor output",ObservableBuffer(activeSensor.values.map{case (x,y) => XYChart.Data[Number,Number](x,y)}))
            textFieldCurrentStateInfo.text = "Connecting..."
            button.text = "Stop"
            setDisabled(comboBox,sliderLower,sliderUpper)
            activeSensor.simStart(plot,textFieldCurrentStateInfo,Array(sliderLower.value.toInt,sliderUpper.value.toInt))
          case _ =>
            button.text = "Start"
            setEnabled(comboBox,sliderLower,sliderUpper)
            activeSensor.simStop(plot,textFieldCurrentStateInfo)
        }
      }
      comboBox.onAction = (ae: ActionEvent) => {
        sensors.foreach(_.clearSequence())
        textFieldCurrentStateInfo.text = "Press start"
        getActiveSensor(comboBox.value.value, sensors) match {
          case Some(kind) =>
            yAxis.label = s"${kind.name} (${kind.unit})"
            plot.title = s"${kind.name} - Time Graph"
          case None =>
            yAxis.label = s"${sensors.head.name} (${sensors.head.unit})"
            plot.title = s"${sensors.head.name} - Time Graph"
        }
        resetSliders(sliderLower, sliderUpper, textFieldLowerValue, textFieldUpperValue, comboBox.value.value.toString, sensors)
      }
      sliderLower.value.onChange { (_, _, newValue) => {
        adjustSliders(newValue.intValue(),sliderLower,sliderUpper,textFieldLowerValue,textFieldUpperValue,comboBox.value.value.toString,sensors)
      }}
      sliderUpper.value.onChange { (_, _, newValue) => {
        adjustSliders(newValue.intValue(),sliderUpper,sliderLower,textFieldUpperValue,textFieldLowerValue,comboBox.value.value.toString,sensors)
      }}

      /* Show GUI window */
      content = List(button, comboBox, plot, sliderLower, sliderUpper, textFieldLowerInfo, textFieldUpperInfo, textFieldCurrentStateInfo, textFieldLowerValue, textFieldUpperValue)
    }
  }

  private def setDisabled(args: Control*): Unit = args.foreach(_.disable = true)
  private def setEnabled(args: Control*): Unit = args.foreach(_.disable = false)
  private def setUneditable(args: TextField*): Unit = args.foreach( e => {
    e.editable = false
    e.focusTraversable = false
  })
  private def setPosition(e: Region, x: Int, y: Int) : Unit = {
    e.layoutX = x
    e.layoutY = y
  }
  private def resetSliders(sliderLower: Slider, sliderUpper: Slider, textLower: TextField, textUpper: TextField, comboBoxValue: String, sensors: Vector[Sensor with Product with Serializable]): Unit = {
    getActiveSensor(comboBoxValue, sensors) match {
      case Some(s) =>
        sliderLower.min = s.minVal.toInt
        sliderLower.max = s.maxVal.toInt - 1
        sliderLower.value = s.minVal.toInt
        sliderUpper.min = s.minVal.toInt + 1
        sliderUpper.max = s.maxVal.toInt
        sliderUpper.value = s.maxVal.toInt
        textLower.text = s"${sliderLower.value.value} ${s.unit}"
        textUpper.text = s"${sliderUpper.value.value} ${s.unit}"
      case None =>
    }
  }
  private def getActiveSensor(comboBoxValue: String, sensors: Vector[Sensor with Product with Serializable]): Option[Sensor] = sensors.find(_.name == comboBoxValue)
  private def adjustSliders(newVal: Int, sliderChanged: Slider, sliderOther: Slider, textSliderChanged: TextField, textSliderOther: TextField, comboBoxValue: String, sensors: Vector[Sensor with Product with Serializable]): Unit ={
    getActiveSensor(comboBoxValue, sensors) match {
      case Some(s) =>
        // TODO Filter values and reject unwanted
        sliderChanged.value = newVal
        textSliderChanged.text = s"${sliderChanged.value.value} ${s.unit}"
      case None =>
    }
  }
  protected def parseSequenceToPlottable(name: String, seq: Seq[(Double,Double)]): XYChart.Series[Number, Number] = {
    XYChart.Series[Number,Number](name, ObservableBuffer(seq.map{case (x,y) => XYChart.Data[Number,Number](x,y)}))
  }
}
