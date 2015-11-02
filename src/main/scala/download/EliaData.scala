package download

import org.joda.time.DateTime

class EliaData(val time : DateTime, val forecast : Double, val actual : Double, val capacity : Double) {
  val capacityFactor = actual/capacity  
}
