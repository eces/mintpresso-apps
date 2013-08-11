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
// PushNotification (long polling)

class PickupActor extends Actor {
  def receive = {

    case Webhook(url, method, json, key, orderKey, timestamp) => {
      val oldState = Cache.getAs[String](s"${key} state").getOrElse("paused")
      Cache.set(s"${key} state", "hold")

      if(json){
        Cache.getAs[String](s"${orderKey} json").getOrElse(Json.obj("message" -> "pickup.cache.empty"))
      }else{
        Cache.getAs[String](s"${orderKey} raw").getOrElse("pickup.cache.empty")
      }

      Cache.set(s"${key} state", oldState)
    }

    case _ => 
      // error
  }
}