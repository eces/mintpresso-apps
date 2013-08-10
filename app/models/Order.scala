package models

import play.api.mvc._
import play.api.Play.current
import play.api.db._
import anorm._ 
import anorm.SqlParser._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.i18n.Messages
import play.api.Play.current
import play.api.Logger
import java.util.Date

case class Order(var no: Long, var title: String, var state: String,
  var createdAt: Date, var updatedAt: Date, var referencedAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$no"         -> this.no,
      "title"       -> this.title,
      "state"       -> this.state,
      "$createdAt"  -> this.createdAt,
      "$updatedAt"  -> this.updatedAt,
      "$referencedAt" -> this.referencedAt
    )
  }

  def toTypedJson: JsObject = {
    Json.obj(
      "order" -> this.toJson
    )
  }

  def save: Option[Long] = {
    val n = Node(this.toTypedJson)
    // app@mintpresso.com
    n.ownerNo = 1
    n.save
  }

  def delete: Boolean = {
    Node.delete(this.no)(User.Default)
  }

  def prepare: Boolean = {
    // load plan

    // match procedure

    // send scheduled message on Akka to OrderActor.

    // change running state of order.
    this.state = "running"
    this.save match {
      case Some(_) => true
      case None => false
    }
  }

  def cancel: Boolean = {
    // send stop command to Actor.
    
    // change running state to paused.
    this.state = "paused"
    this.save match {
      case Some(_) => true
      case None => false
    }
  }
}

object Order {

  def apply(json: JsValue): Order = {
    val key = (json \ "order").as[JsObject]
    Order(
      (key \ "$no").as[Long],
      (key \ "title").as[String],
      (key \ "state").as[String],
      (key \ "$createdAt").as[Date],
      (key \ "$updatedAt").as[Date],
      (key \ "$referencedAt").as[Date]
    )
  }

}