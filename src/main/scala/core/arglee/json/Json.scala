
package core.arglee.json

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



object Json extends BaseJson {

  object Type extends Enumeration {
    val NULL, INT, DOUBLE, BOOLEAN, STRING, ARRAY, OBJECT = Value
  }
}

class BaseJson() {
  def apply(json: String): Convertors = {
    val str = ArrayBuffer[(Char, Any)]()
    var i = 0

    while (i < json.length) {
      if (json(i).isWhitespace || json(i) == '-' || json(i) == ':' || json(i) == ',') {
      } else if (json(i) == '[') {
        str.append(('[', null))
      } else if (json(i) == '{') {
        str.append(('{', null))
      } else if (json(i) == ']') {
        val vec = mutable.ArrayStack[Convertors]()
        while (str.nonEmpty && str.last._1 != '[') {
          vec.push(Convertors(str.last._2))
          str.trimEnd(1)
        }
        if (str.isEmpty || str.last._1 != '[') {
          throw new Exception("parse error, [] not match")
        }
        str.trimEnd(1)
        str.append(('a', vec.iterator))
      } else if (json(i) == '}') {
        val now = mutable.HashMap[String, Convertors]()

        while (str.length >= 2 && str.last._1 != '{') {
          val new_value: Convertors = Convertors(str.last._2)
          str.trimEnd(1)
          val new_key = str.last._2.asInstanceOf[String]
          str.trimEnd(1)
          now.update(new_key, new_value)
        }
        if (str.isEmpty || str.last._1 != '{') {
          throw new Exception("parse error, {} not match")
        }
        str.trimEnd(1)
        str.append(('o', now.toMap))
      } else if (json(i) == '"') {
        var j = i + 1
        val S = new mutable.StringBuilder()
        while (j < json.length && json(j) != '"') {
          if (json(j) == '\\') {
            if (json(j + 1) == 'b') {
              S.append('\b')
            } else if (json(j + 1) == 'f') {
              S.append('\f')
            } else if (json(j + 1) == 'n') {
              S.append('\n')
            } else if (json(j + 1) == 'r') {
              S.append('\r')
            } else if (json(j + 1) == 't') {
              S.append('\t')
            } else {
              S.append(json(j + 1))
            }
            j += 2
          } else {
            S.append(json(j))
            j += 1
          }
        }
        str.append(('v', S.toString()))
        i = j
      } else if (json(i).isDigit) {
        val is_double = {
          var j = i + 1
          while (j < json.length && json(j).isDigit) {
            j += 1
          }
          j < json.length && (json(j) == '.' || json(j) == 'e' || json(j) == 'E')
        }
        val minus_flag = if (i > 0 && json(i - 1) == '-') {
          -1
        } else {
          1
        }
        var j = i
        while (j < json.length && json(j) != ',' && json(j) != ']' && json(j) != '}') {
          j += 1
        }
        if (is_double) {
          val v = json.substring(i, j).toDouble * minus_flag
          str.append(('d', v))
        } else {
          val v = json.substring(i, j).replaceAll(" ", "").toLong * minus_flag
          str.append(('i', v))
        }
        i = j - 1
      } else if (json.substring(i, i + 4) == "null") {
        str.append(('n', null))
        i += 3
      } else if (json.substring(i, i + 4) == "true") {
        str.append(('b', true))
        i += 3
      } else if (json.substring(i, i + 5) == "false") {
        str.append(('b', false))
        i += 4
      } else {
        throw new Exception("parse error:unrecognized character##" + json.substring(i) + "##pos:" + i)
      }
      i += 1
    }
    if (str.length != 1) {
      throw new Exception("parse error, type=final")
    }
    Convertors(str.head._2)
  }
}
