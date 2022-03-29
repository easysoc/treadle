// SPDX-License-Identifier: Apache-2.0

package treadle.executable

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{pretty, render}

case class WaveformValues(
  var clockValues:  Array[BigInt],
  symbols:      Array[Symbol],
  var symbolValues: Array[Array[BigInt]] // should be dimensions of (numSymbols, numCycles)
) {

  if (clockValues.length % 2 != 0) {
    clockValues = clockValues.drop(1)
    symbolValues.zipWithIndex.foreach { case (arr, i) =>
      symbolValues(i) = arr.drop(1)
    }
  }

  val numSymbols = symbols.length
  val numCycles = clockValues.length

  override def toString: String = {
    symbolValues.map(_.mkString(" ")).mkString("\n")
  }

  def toJson: JValue = {
    if (numCycles < 2) {
      val json: JValue = JObject()
      json
    } else {
      val clkWaveString = "N" + "." * (numCycles / 2 - 1)
      val waveStrings = Array.fill[String](numSymbols)("")
      val dataStrings = Array.fill[String](numSymbols)("")
      val prevValues = new Array[BigInt](numSymbols)

      symbolValues.zipWithIndex.foreach { case (arr, i) =>
        val needDataField = symbols(i).bitWidth > 1
        arr.foreach { value =>
          if (value == prevValues(i)) {
            waveStrings.update(i, waveStrings(i) + ".")
          } else {
            waveStrings.update(i, waveStrings(i) + (if (needDataField) "2" else value.toString()))
            if (needDataField) dataStrings.update(i, dataStrings(i) + value + " ")
          }
          prevValues.update(i, value)
        }
      }

      // Generate JSON
      val jsonClk: JObject = ("name" -> "clock") ~ ("wave" -> clkWaveString) ~ ("period" -> 2)
      val jsonWaves: JArray = (
        symbols.map { symbol =>
          symbol.name
        }.toList,
        waveStrings.toList,
        dataStrings.toList
        ).zipped.map { case (symbolName, waveString, dataString) =>
        if (dataString.isEmpty) {
          ("name" -> symbolName) ~ ("wave" -> waveString)
        } else {
          ("name" -> symbolName) ~ ("wave" -> waveString) ~ ("data" -> dataString)
        }
      }
      val jsonAllWaves = jsonClk ++ jsonWaves

      val json: JObject = ("signal" -> jsonAllWaves) ~ ("config" -> ("skin" -> "narrow"))
//      val json: JValue = ("signal" -> jsonAllWaves) ~ ("head" -> ("tick" -> 0))
      json
    }
  }

  def toJsonString: String = {
    pretty(render(toJson))
  }
}
