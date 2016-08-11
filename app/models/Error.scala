package models

import play.api.libs.json._

case class Error(id: Long, name: String, date: String)

object Error {
  implicit val errorFormat = Json.format[Error]
}
