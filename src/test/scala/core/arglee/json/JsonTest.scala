package core.arglee.json

import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
  * Created by innovaccer on 17/12/16.
  */
class JsonTest extends FunSuite with BeforeAndAfterEach {

  val json = """{"a":"b","c":"d"}"""

  override def beforeEach() {

  }

  override def afterEach() {

  }

  test("json parse testing") {

    val parsedString = Json(json)
    println(json)
  }

}
