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
import controllers.v2.Pickups

case class Pickup(var no: Long, var title: String, var state: String,
  var resultType: String, var resultQuery: String,
  var plans: List[JsObject], var orderNo: Long,
  var createdAt: Date, var updatedAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$no"         -> this.no,
      "$id"         -> "",
      "title"       -> this.title,
      "state"       -> this.state,
      "resultType"  -> this.resultType,
      "resultQuery" -> this.resultQuery,
      "plans"       -> this.plans,
      "orderNo"     -> this.orderNo,
      "$createdAt"  -> this.createdAt,
      "$updatedAt"  -> this.updatedAt
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

  def addCallback(key: String) = {
    // set hook for resource (rpush)
    var callbacks: String = ""
    Cache.getAs[String](s"${key} callback pickup") match {
      case Some(s: String) => 
        callbacks = (s.split(','):+this.no.toString).toSet.mkString(",")
      case None =>
        callbacks = this.no.toString
    }
    // Logger.debug(s"${key} callback pickup(${this.no}) := ${callbacks.toString}")
    Cache.set(s"${key} callback pickup", callbacks)
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

  def prepare(orderKey: String)(user: User): Boolean = {
    Pickups.log.trace = "models.Pickup.prepare"

    // set timestamp
    val timestamp = new Date().getTime
    val duration = Duration("500 ms")
    val seconds = duration.toMillis
    val pickupKey = "pickup " + this.no

    // start immediately if it was paused.
    // val updatedAt = Cache.getAs[Long](s"${pickupKey} updatedAt").getOrElse( this.updatedAt.getTime )
    
    
    // do something only if scheduled interval is over
    if(this.state != "paused" && (updatedAt.getTime + seconds) > timestamp){
      return true
    }

    // change state
    this.prepare

    Cache.getAs[String](s"${pickupKey} state").getOrElse(this.state) match {
      case "running" | "paused" => {
        // branch by data types
        resultType match {
          case "model" => {
            // fetch plans
            plans.foreach { plan =>
              val key = (plan \ "key").as[String]
              var value = (plan \ "value").as[String]
              key match {
                case "add" =>
                  try { 
                    val json = Json.parse("\"" + value + "\"")
                  } catch {
                    case e: Exception => 
                      Actors.log ! actors.Warn("pickup.json.invalid", Pickups.log, user)
                      // warn
                      value = "value"
                  }
                  Actors.pickup ! ModelUpdateJsonWithKey(value, pickupKey, orderKey, user.no, timestamp)
                  addCallback(orderKey)
                case "subtract" =>
                  val p1 = value.split('~')
                  if(p1.length != 2){
                    // error
                    Actors.log ! actors.Error("pickup.value.invalid", Pickups.log, user)
                    return false
                  }
                  var storeKey = p1(0)
                  var baseKey = p1(1)

                  Actors.pickup ! ModelSubtractJsonWithKey(storeKey, baseKey, pickupKey, orderKey, user.no, timestamp)
                  addCallback(orderKey)
                case _ => 
              }
            }
            true
          }
          case "status" => {
            // fetch plans
            plans.foreach { plan =>
              val key = (plan \ "key").as[String]
              var value = (plan \ "value").asOpt[String].getOrElse("")
              key match {
                case "add" =>
                  try { 
                    if(value.length == 0){
                      value = "value"
                    }
                    val json = Json.parse("\"" + value + "\"")
                  } catch {
                    case e: Exception => 
                      // warn
                      value = "value"
                  }
                  val parts = resultQuery.split(' ')
                  if(parts.length != 3){
                    Actors.log ! actors.Error("pickup.value.invalid", Pickups.log, user)
                    return false
                  }
                  Actors.pickup ! StatusCreateForEach(value, parts(1), pickupKey, orderKey, user.no, timestamp)
                  // Actors.pickup ! ModelUpdateJsonWithKey(value, pickupKey, orderKey, user.no, timestamp)
                  addCallback(orderKey)
                case _ => 
              }
            }
            true
          }
          case _ => false
        }
      }

      // already in progress so let's drop it.
      case "hold" => true
      case _ => false
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
      (key \ "title").as[String],
      (key \ "state").as[String],
      (key \ "resultType").as[String],
      (key \ "resultQuery").asOpt[String].getOrElse(""),
      (key \ "plans").as[List[JsObject]],
      (key \ "orderNo").as[Long],
      (key \ "$createdAt").as[Date],
      (key \ "$updatedAt").as[Date]
    )
  }

}