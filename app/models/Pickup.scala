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
  var resultType: String, var resultQuery: JsObject,
  var createdAt: Date, var updatedAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$no"         -> this.no,
      "$id"         -> this.id,
      "title"       -> this.title,
      "state"       -> this.state,
      "resultType"  -> this.resultType,
      "resultQuery" -> this.resultQuery,
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

  def prepare(orderKey: String)(user: User) = {
    // set timestamp
    val timestamp = new Date().getTime
    val duration = Duration("10 seconds")

    // start immediately if it was paused.
    val updatedAt = Cache.getAs[Long](s"pickup ${this.no} updatedAt").getOrElse(0L)
    Cache.getAs[String](s"pickup ${this.no} state").getOrElse(this.state) match {
      case "paused" | "running" => {
        // do something only if scheduled interval is over
        if((updatedAt + duration.toSeconds) <= timestamp){
          // load plan
          // resultType match {
          //   case "status-create" =>
          //     val parts = (resultQuery \ "format").as[String].split(' ')
          //     if(parts.length != 3){
          //       // error
          //       false
          //     }else{
          //       user no listen music
          //       music 
          //       val sTypeNo = Type(parts(0)).no
          //       val oTypeNo = Type(parts(2)).no
          //       val jsonParts = parts(1).split(':')
          //       val v = jsonParts(0)
          //       val edgeJson = jsonParts(1)
          //     }
          //     Actors.pickup ! StatusCreateEachWithVerbAndJson(
          //       v, edgeJson, s"pickup ${this.no}", orderKey, user.no, timestamp)
              
            // case _ =>
            //   // error
          // }
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
      (key \ "resultType").as[String],
      (key \ "resultQuery").asOpt[JsObject].getOrElse(Json.obj()),
      (key \ "$createdAt").as[Date],
      (key \ "$updatedAt").as[Date]
    )
  }

}