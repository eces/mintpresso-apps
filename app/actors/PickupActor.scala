package actors

import play.api._
import play.api.cache._
import play.api.libs._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Play.current
import play.api.i18n.Messages
import akka.actor.{Actor, Props, ActorSystem}
import akka.actor.Actor._
import scala.concurrent._
import scala.concurrent.duration._
import models._

case class Webhook(url: String, method: String, json: Boolean, key: String, orderKey: String, timestamp: Long)
case class StatusCreateWithTypesAndVerb(sTypeNo: Long, oTypeNo: Long, v: String, edgeJson: String, pickupKey: String, orderKey: String, userNo: Long, timestamp: Long)
case class StatusCreateForEach(jsonFieldName: String, v: String, pickupKey: String, orderKey: String, userNo: Long, timestamp: Long)
case class ModelUpdateJsonWithKey(jsonFieldName: String, pickupKey: String, orderKey: String, userNo: Long, timestamp: Long)
case class PickupCallback(pickup: Pickup, orderKey: String, user: User)
// PushNotification (long polling)

class PickupActor extends Actor {
  def receive = {

    case PickupCallback(pickup, orderKey, user) => {
      // Logger.debug("Pickup Callback - " + pickup.no)
      pickup.prepare(orderKey)(user)
    }

    case ModelUpdateJsonWithKey(jsonFieldName, pickupKey, orderKey, userNo, timestamp) => {
      val oldState = Cache.getAs[String](s"${pickupKey} state").getOrElse("paused")
      Cache.set(s"${pickupKey} state", "hold")

      val updatedAt = Cache.getAs[Long](s"${pickupKey} updatedAt").getOrElse(0L)
      // skip if pickup has been updated to newer result.
      if(updatedAt > timestamp){
        // info - report to app@mintpresso.com
      }else{
        Cache.getAs[String](s"${orderKey} json-kv") match {
          case Some(values) =>
            Json.parse(values).as[List[JsObject]].foreach { kv =>
              val key = (kv \ "key").as[Long]
              val value = (kv \ "value").as[Long]
              Node.findOneByNo(key) map { node =>
                node.updatedAt = new java.util.Date
                node.json = node.json ++ Json.obj( jsonFieldName -> value )
                node.save

                // respond callback
                node.callback(User.Empty(userNo))

              } getOrElse {
                // error
              }
            }
          case None =>
            // warn
        }
      }
      Cache.set(s"${pickupKey} state", oldState)
    }

    case StatusCreateForEach(jsonFieldName, v, pickupKey, orderKey, userNo, timestamp) => {
      val oldState = Cache.getAs[String](s"${pickupKey} state").getOrElse("paused")
      Cache.set(s"${pickupKey} state", "hold")

      val updatedAt = Cache.getAs[Long](s"${pickupKey} updatedAt").getOrElse(0L)
      // skip if pickup has been updated to newer result.
      if(updatedAt > timestamp){
        // info - report to app@mintpresso.com
      }else{
        Cache.getAs[String](s"${orderKey} json-list") match {
          case Some(values) =>
            Json.parse(values).as[List[JsObject]].foreach { kv =>
              val s = (kv \ "$subject").as[Long]
              val o = (kv \ "$object").as[Long]
              val e = Edge( 0L, userNo, Node.findOneByNo(s).get, v, Node.findOneByNo(o).get, Json.obj( jsonFieldName -> (kv \ "value").as[JsValue] ))
              e.save
              e.callback(User.Empty(userNo))
            }
          case None =>
            // warn
        }
      }

      // val value: String = Cache.getAs[String](s"${orderKey} raw").getOrElse("null")
      // edgeJson.replace("$value", value)

      // try { 
      //   val j = Json.parse(edgeJson)
      // Edge( 0L, userNo, sTypeNo, "issue", Node.findOneByNo(key.no).get).save
      //   Logger.info("NotImplemented - StatusCreateWithTypesAndVerb")
      //   // ...
      // } catch {
      //   case e: Exception => 
      //     // error
      //     // pause
      // }
      

      Cache.set(s"${pickupKey} state", oldState)
    }

    case _ => 
      // error
  }
}