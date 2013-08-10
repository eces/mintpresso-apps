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

case class Pickup(var no: Long, var id: String, var title: String, var state: String,
  var createdAt: Date, var updatedAt: Date, var referencedAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$no"         -> this.no,
      "$id"         -> this.id,
      "title"       -> this.title,
      "state"       -> this.state,
      "$createdAt"  -> this.createdAt,
      "$updatedAt"  -> this.updatedAt,
      "$referencedAt" -> this.referencedAt
    )
  }

  def toTypedJson: JsObject = {
    Json.obj(
      "pickup" -> this.toJson
    )
  }

  def save: Option[Long] = {
    val n = Node(Json.obj( "pickup" -> this.toJson ))
    // app@mintpresso.com
    n.ownerNo = 1
    n.save
  }

  def delete: Boolean = {
    Node.delete(this.no)(User.Default)
  }

  def prepare = {
    // load plan

    // match procedure

    // send scheduled message on Akka to PickupActor.

    // change running state of pickup.
    this.state = "running"
    this.save match {
      case Some(_) => true
      case None => false
    }
  }

  def cancel = {
    // send stop command to Actor.
    
    // change running state to paused.
    this.state = "paused"
    this.save match {
      case Some(_) => true
      case None => false
    }
  }
}

object Pickup {

  def apply(json: JsValue): Pickup = {
    val key = (json \ "pickup").as[JsObject]
    Pickup(
      (key \ "$no").as[Long],
      (key \ "$id").as[String],
      (key \ "title").as[String],
      (key \ "state").as[String],
      (key \ "$createdAt").as[Date],
      (key \ "$updatedAt").as[Date],
      (key \ "$referencedAt").as[Date]
    )
  }

}