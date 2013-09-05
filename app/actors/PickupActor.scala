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
// PushNotification (long polling)

class PickupActor extends Actor {
  def receive = {

    case StatusCreateWithTypesAndVerb(sTypeNo, oTypeNo, v, edgeJson, pickupKey, orderKey, userNo, timestamp) => {
      // val oldState = Cache.getAs[String](s"${pickupKey} state").getOrElse("paused")
      // Cache.set(s"${pickupKey} state", "hold")

      // val value: String = Cache.getAs[String](s"${orderKey} raw").getOrElse("null")
      // edgeJson.replace("$value", value)

      // try { 
      //   val j = Json.parse(edgeJson)
      //   // Edge( 0L, userNo, sTypeNo, "issue", Node.findOneByNo(key.no).get).save
      //   Logger.info("NotImplemented - StatusCreateWithTypesAndVerb")
      //   // ...
      // } catch {
      //   case e: Exception => 
      //     // error
      //     // pause
      // }
      

      // Cache.set(s"${pickupKey} state", oldState)
      
    }

    case _ => 
      // error
  }
}