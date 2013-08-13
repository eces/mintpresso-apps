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
  var json: JsObject,
  var createdAt: Date, var updatedAt: Date, var referencedAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$no"         -> this.no,
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
    var newSet: Set[Long] = Set(this.no)
    Cache.getAs[Set[Long]](s"${key} callback order") match {
      case Some(s: Set[Long]) => newSet ++= s
      case None =>
    }
    Cache.set(s"${key} callback order", newSet)
  }

  def prepare: Boolean = {
    // change running state of order.
    this.state = "running"
    this.save match {
      case Some(_) => 
        Cache.remove(s"order ${this.no} state")
        true
      case None => false
    }
  }

  def prepare(implicit user: User) = {
    // set timestamp
    val timestamp = new Date().getTime
    val duration = Duration((this.json \ "schedule").as[String])

    // start immediately if it was paused.
    val updatedAt = Cache.getAs[Long](s"order ${this.no} updatedAt").getOrElse(0L)
    Cache.getAs[String](s"order ${this.no} state").getOrElse(this.state) match {
      case "paused" | "running" => {
        // do something if scheduled interval is over
        if((updatedAt + duration.toSeconds) <= timestamp){
          // load plan
          (this.json \ "plan").as[String] match {
            // match procedure
            case "NodeCount" => {
              val typeNo = Type((this.json \ "nodeType").as[String]).no

              // send scheduled message on Akka to OrderActor.
              Actors.order ! NodeCount(typeNo, "order ${this.no}", user.no, timestamp)

              // set hook for resource (rpush)
              this.addCallback(s"${user.no} node typeNo:${typeNo}")
            }
            // case "NodeCountWithJson" => {
            //   val typeNo = Type((this.json \ "nodeType").as[String]).no
            //   val condition = (this.json \ "condition").as[String]
            //   Actors.order ! NodeCount(typeNo, condition, "order ${this.no}", user.no, timestamp)
            // }
            case "EdgeCountWithTypesAndVerb" => {
              val sTypeNo = Type((this.json \ "sType").as[String]).no
              val oTypeNo = Type((this.json \ "oType").as[String]).no
              val v = (this.json \ "sType").as[String]
              Actors.order ! EdgeCountWithTypesAndVerb(sTypeNo, oTypeNo, v, "order ${this.no}", user.no, timestamp)
              // this.addCallback(s"${user.no} edge sTypeNo:${sTypeNo} oTypeNo:${oTypeNo}")
              this.addCallback(s"${user.no} edge v:${v}")
            }
            case _ => {
              // error
            }
          }
        }
      }

      // already in progress so let's drop it.
      case "hold" =>
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
      (key \ "json").as[JsObject],
      (key \ "$createdAt").as[Date],
      (key \ "$updatedAt").as[Date],
      (key \ "$referencedAt").as[Date]
    )
  }

}