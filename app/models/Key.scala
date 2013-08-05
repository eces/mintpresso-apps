package models

import play.api.mvc._
import play.api.libs.json._
import play.api.i18n.Messages
import play.api.Play.current
import play.api.Logger
import play.api.libs._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent._
import scala.concurrent.duration._
import java.util.Date

case class Key(var id: String, var no: Long, var ownerNo: Long, var url: List[String], 
	var address: List[String], var createdAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$id" ->        this.id,
      "$no" ->        this.no,
      "ownerNo" ->    this.ownerNo,
      "url" ->        this.url,
      "address" ->    this.address
    )
  }
}

object Key {
  def apply(json: JsValue): Key = {
    val key = (json \ "key").as[JsObject]
    Key(
      (key \ "$id").as[String],
      (key \ "$no").as[Long],
      (key \ "ownerNo").as[Long],
      (key \ "url").as[List[String]],
      (key \ "address").as[List[String]],
      (key \ "$createdAt").as[Date]
    )
  }
  def findByNo(no: Long): Option[Key] = {
    Logger.info("Not implemented")
    None
  }
}