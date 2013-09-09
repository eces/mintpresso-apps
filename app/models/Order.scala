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

case class Order(var no: Long, var title: String, var state: String,
  var dataType: String, var dataQuery: String,
  var plans: List[JsObject], var duration: String,
  var createdAt: Date, var updatedAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$no"         -> this.no,
      "$id"         -> "",
      "title"       -> this.title,
      "state"       -> this.state,
      "dataType"    -> this.dataType,
      "dataQuery"   -> this.dataQuery,
      "plans"       -> this.plans,
      "duration"    -> this.duration,
      "$createdAt"    -> this.createdAt,
      "$updatedAt"    -> this.updatedAt
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

  def addCallback(key: String) = {
    // set hook for resource (rpush)
    var callbacks: String = ""
    Cache.getAs[String](s"${key} callback order") match {
      case Some(s: String) => 
        callbacks = (s.split(','):+this.no.toString).toSet.mkString(",")
      case None =>
        callbacks = this.no.toString
    }
    Cache.set(s"${key} callback order", callbacks)
    // Logger.debug(s"${key} callback order := ${callbacks.toString}")
  }

  def prepare: Boolean = {
    // change running state of order.
    this.state = "running"
    this.save match {
      case Some(_) => 
        Cache.set(s"order ${this.no} state", "running")
        true
      case None =>
        Cache.remove(s"order ${this.no} state")
        false
    }
  }

  def prepare(user: User): Boolean = {
    // set timestamp
    val timestamp = new Date().getTime
    val duration = Duration(this.duration)
    val orderKey = "order " + this.no

    // start immediately if it was paused.
    val updatedAt = Cache.getAs[Long](s"${orderKey} updatedAt").getOrElse(0L)
    
    // do something only if scheduled interval is over
    if((updatedAt + duration.toSeconds) > timestamp){
      return true
    }

    Logger.debug("Prepare")
    Cache.getAs[String](s"${orderKey} state").getOrElse(this.state) match {
      case "running" | "paused" => {
        // branch by data types
        dataType match {
          case "status" => {
            // fetch plans
            plans.foreach { plan =>
              val key = (plan \ "key").as[String]
              var value = (plan \ "value").as[String]
              key match {
                case "count" =>
                  val parts = dataQuery.split(' ')
                  if(parts.length != 3){
                    // error
                    return false
                  }else{
                    val sTypeNo = Type(parts(0)).no
                    val v = parts(1)
                    val oTypeNo = Type(parts(2)).no

                    // val AlphabetPattern = "([a-z0-9]+)".r
                    // var jsonMatcher = false
                    // transform value to query
                    // value match {
                    //   case "$no" =>
                    //     value = "no"
                    //   case Json.parse(v) =>
                    //     value = s"\"${v}\":"
                    //     jsonMatcher = true
                    //   case _ => 
                    //     // info
                    //     value = "no"
                    // }
                    var column = ""
                    var groupBy = ""
                    value match {
                      case "s" =>
                        // column = "`s` as `key`,"
                        // groupBy = "GROUP BY `s`"
                      case "o" => 
                        // column = "`o` as `key`,"
                        // groupBy = "GROUP BY `o`"
                      case "v" => 
                      case _ => 
                        // error
                        return false
                    }

                    // change state
                    this.prepare

                    Actors.order ! EdgeCountWithTypesAndVerb(
                      sTypeNo, oTypeNo, v, value, s"order ${this.no}", user.no, timestamp)
                    // if(jsonMatcher){
                    //   Actors.order ! EdgeCountWithTypesAndVerbAndJson(sTypeNo, oTypeNo, v, value, s"order ${this.no}", user.no, timestamp)
                    // }else{
                    // }
                    addCallback(s"${user.no} edge v:${v}")
                    return true
                  }
                case _ => 
                  // error 
              }
            }
            true
          }
          case "model" => false
          case _ => {
            // error
            false
          } 
        }
      }

      // already in progress so let's drop it.
      case "hold" => true
      case _ => false
    }
  }

  def cancel: Boolean = {
    // send stop command to Cache state.
    
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
      (key \ "dataType").as[String],
      (key \ "dataQuery").as[String],
      (key \ "plans").as[List[JsObject]],
      (key \ "duration").as[String],
      (key \ "$createdAt").as[Date],
      (key \ "$updatedAt").as[Date]
    )
  }

}