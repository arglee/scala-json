
package core.arglee.json

import scala.collection.mutable

/**
  * A collection to convert json string into various different forms.
  */



class Convertors(inputString: Any) {

  def asInt: Int = value match {
    case v: Long if v.toInt == v => v.toInt
  }

  def isInt: Boolean = value.isInstanceOf[Long] || value.isInstanceOf[Int]

  def asLong = value.asInstanceOf[Long]

  def asDouble = value.asInstanceOf[Double]

  def isDouble = value.isInstanceOf[Double]

  def asBoolean = value.asInstanceOf[Boolean]

  def isBoolean = value.isInstanceOf[Boolean]

  def asString = value.asInstanceOf[String]

  def isString = value.isInstanceOf[String]

  def asArray = value.asInstanceOf[Array[Convertors]]

  def isArray = value.isInstanceOf[Array[_]]

  def asMap = value.asInstanceOf[Map[String, Convertors]]

  def isMap = value.isInstanceOf[Map[_, _]]

  def apply(i: Int): Convertors= value.asInstanceOf[Array[Convertors]](i)

  def apply(key: String): Convertors= value.asInstanceOf[Map[String, Convertors]](key)

  def write(): String = {
    val buffer = new mutable.StringBuilder()
    rec_write(buffer)
    buffer.toString()
  }

  private val value: Any = inputString match {
    case null => null
    case v: Int => v.toLong
    case v: Long => inputString
    case v: Double => inputString
    case v: Boolean => inputString
    case v: String => inputString
    case v: Convertors=> v.value
    case v: Map[_, _] =>
      v.map { case one =>
        (one._1.toString, Convertors(one._2))
      }
    case v: Vector[_] => v.map(Convertors(_)).toArray
    case v: List[_] => v.map(Convertors(_)).toArray
    case v: Array[_] => v.map(Convertors(_))
    case v: Iterator[_] => v.map(Convertors(_)).toArray

    case _ => throw new Exception("unknow type")
  }

  private def rec_write(buffer: mutable.StringBuilder): Unit = {
    value match {
      case null => buffer.append("null")
      case v: Long => buffer.append(v)
      case v: Boolean => buffer.append(v)
      case v: Double => buffer.append(v)
      case v: String =>
        buffer.append('"')
        v.foreach {
          each => {
            if (each == '\\' || each == '"') {
              buffer.append('\\')
            } else if (each == '\b') {
              buffer.append("\\b")
            } else if (each == '\f') {
              buffer.append("\\f")
            } else if (each == '\n') {
              buffer.append("\\n")
            } else if (each == '\r') {
              buffer.append("\\r")
            } else if (each == '\t') {
              buffer.append("\\t")
            } else {
              buffer.append(each)
            }
          }
        }
        buffer.append('"')
      case v: Array[_] =>
        buffer.append('[')
        for (i <- v.indices) {
          if (i != 0) {
            buffer.append(',')
          }
          v(i).asInstanceOf[Convertors].rec_write(buffer)
        }
        buffer.append(']')
      case v: Map[_, _] =>
        buffer.append('{')
        var first = true
        v.foreach {
          case one =>
            if (!first) {
              buffer.append(',')
            }
            first = false
            buffer.append('"')
            buffer.append(one._1)
            buffer.append('"')
            buffer.append(':')
            one._2.asInstanceOf[Convertors].rec_write(buffer)
        }
        buffer.append('}')
      case _ => throw new Exception("unknow data type")
    }
  }
  
}


object Convertors {
  def apply(inputString: Any): Convertors = new Convertors(inputString)
}