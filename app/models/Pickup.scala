package models

import play.api.mvc._
import play.api.cache._
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
import scala.concurrent.duration._
import actors._

case class Pickup(var no: Long, var id: String, var title: String, var state: String,
  var json: JsObject,
  var createdAt: Date, var updatedAt: Date, var referencedAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$no"         -> this.no,
      "$id"         -> this.id,
      "title"       -> this.title,
      "state"       -> this.state,
      "json"        -> this.json,
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
    val n = Node(this.toTypedJson)
    // app@mintpresso.com
    n.ownerNo = 1
    n.save
  }

  def delete: Boolean = {
    Node.delete(this.no)(User.Default)
  }

  def prepare: Boolean = {
    // change running state of order.
    this.state = "running"
    this.save match {
      case Some(_) => 
        Cache.remove(s"pickup ${this.no} state")
        true
      case None => false
    }
  }

  def prepare(orderKey: String) = {
    // set timestamp
    val timestamp = new Date().getTime
    val duration = Duration((this.json \ "schedule").as[String])

    // start immediately if it was paused.
    val updatedAt = Cache.getAs[Long](s"pickup ${this.no} updatedAt").getOrElse(0L)
    Cache.getAs[String](s"pickup ${this.no} state").getOrElse(this.state) match {
      case "paused" | "running" => {
        // do something if scheduled interval is over
        if((updatedAt + duration.toSeconds) <= timestamp){
          // load plan
          (this.json \ "plan").as[String] match {
            // match procedure
            case "Webhook" => {
              val url = (this.json \ "url").as[String]
              val method = (this.json \ "method").as[String]
              val json = (this.json \ "json").as[Boolean]
              // send scheduled message to PickupActor.
              Actors.pickup ! Webhook(url, method, json, s"pickup ${this.no}", orderKey, timestamp)
            }
            case _ => 
              // error
          }
        }
      }

      // already in progress so let's drop it.
      case "hold" =>
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
      (key \ "json").as[JsObject],
      (key \ "$createdAt").as[Date],
      (key \ "$updatedAt").as[Date],
      (key \ "$referencedAt").as[Date]
    )
  }

}