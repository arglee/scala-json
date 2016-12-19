
/**
  * Created by arglee on 16/12/16.
  */

package core.arglee.json

import scala.collection.mutable

class Parser {
  val string = """{"a":"b","c":"d"}"""
  parse(string)

  def parse[T](json: String): Unit = {

    val result: mutable.Seq[(Char, Any)] = mutable.ArrayBuffer[(Char, Any)]()


        json.foreach { case item =>
          println(item)
        }

  }
}
