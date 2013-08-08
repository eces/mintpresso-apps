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
	var address: List[String], var scope: List[String], var logThreshold: String,
  var createdAt: Date, var updatedAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$id" ->        this.id,
      "$no" ->        this.no,
      "ownerNo" ->    this.ownerNo,
      "url" ->        this.url,
      "address" ->    this.address,
      "scope" ->      this.scope,
      "logThreshold"-> this.logThreshold,
      "$createdAt"  -> this.createdAt,
      "$updatedAt"  -> this.updatedAt
    )
  }

  def save: Option[Long] = {
    val k = Node(Json.obj( "apikey" -> this.toJson ))
    // app@mintpresso.com
    k.ownerNo = 1
    k.save
  }

  def delete: Boolean = {
    Node.delete(this.no)
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
      (key \ "scope").as[List[String]],
      (key \ "logThreshold").as[String],
      (key \ "$createdAt").as[Date],
      (key \ "$updatedAt").as[Date]
    )
  }

  def findOneByNo(no: Long)(implicit user: User): Option[Key] = {
    Node.findOneByNo(no) map { n =>
      Some(Key(n.toJson))
    } getOrElse{
      None
    }
  }

  def findOneById(id: String)(implicit user: User): Option[Key] = {
    Node.findOneById(id) map { n =>
      Some(Key(n.toJson))
    } getOrElse{
      None
    }
  }
}