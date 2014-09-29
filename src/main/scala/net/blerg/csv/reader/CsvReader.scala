/*
* Copyright 2014 Arthur Embleton
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package net.blerg.csv.reader

import java.io._
import scala.annotation.tailrec
import scala.io._

class CsvReader(file: File) {
  private val iterator = Source.fromFile(file).getLines()

  @tailrec
  private def parseLine(remainingLine:List[Char], elementValue:String, insideQuotes:Boolean, escapeNext:Boolean, elements:Seq[String]): Seq[String] = remainingLine match {
    case Nil => elements :+ elementValue
    case head :: tail => (head, insideQuotes, escapeNext) match {
      case (char, false, false) => char match {
        case '\\' => parseLine(tail, elementValue, insideQuotes, true, elements)
        case '"' => parseLine(tail, elementValue, true, escapeNext, elements)
        case ',' => parseLine(tail, "", insideQuotes, escapeNext, elements :+ elementValue)
        case _ => parseLine(tail, elementValue + char, insideQuotes, escapeNext, elements)
      }
      case (char, true, false) => char match {
        case '\\' => parseLine(tail, elementValue, insideQuotes, true, elements)
        case '"' => parseLine(tail, elementValue, false, escapeNext, elements)
        case _ => parseLine(tail, elementValue + char, insideQuotes, escapeNext, elements)
      }
      case (_, _, true) => parseLine(tail, elementValue, insideQuotes, false, elements)
    }
  }

  private def parseLine(line:String): Seq[String] = parseLine(line.toList, "", false, false, Seq.empty)

  def forEach(f: Seq[String] => Unit) = iterator.foreach(line => f(parseLine(line)))
}

object CsvReader {
  def open(file: File) = new CsvReader(file)
}