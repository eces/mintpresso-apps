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

case class NodeCount(typeNo: Long, key: String, no: Long, timestamp: Long)
// case class NodeCount(typeNo: Long, condition: String, key: String, no: Long, timestamp: Long)
// case class EdgeCountWithTypesAndVerbAndJson(sTypeNo: Long, oTypeNo: Long, v: String, column: String, groupBy: String, cacheKey: String, userNo: Long, timestamp: Long)
case class EdgeCountWithTypesAndVerb(sTypeNo: Long, oTypeNo: Long, v: String, value: String, cacheKey: String, userNo: Long, timestamp: Long)
case class OrderCallback(order: Order, user: User)
// case class RowCount
// case class Rank
// case class Clean
// case class FrequentRelationCount
// case class AR
// case class CF

class OrderActor extends Actor {
  def receive = {

    case OrderCallback(order, user) => {
      // Logger.debug("Order Callback - " + order.no)
      order.prepare(user)
    } 

    case NodeCount(typeNo, key, no, timestamp) => {
      // val oldState = Cache.getAs[String](s"${key} state").getOrElse("paused")
      // Cache.set(s"${key} state", "hold")

      // implicit val user = User.Empty(no)
      // val count = Node.countAllByTypeNo(typeNo)

      // Cache.set(s"${key} raw", count)
      // Cache.set(s"${key} json", Json.obj( "count" -> count ).toString )
      // Cache.set(s"${key} state", oldState)

      // Cache.getAs[Long](s"${key} callback pickup") map { nodeNo =>
      //   Node.findOneByNo(nodeNo) match {
      //     case Some(node) => 
      //       // pass empty user implicitly
      //       Pickup(node.toTypedJson).prepare
      //     case None =>
      //       // warn
      //   }
      // }
    }

    case EdgeCountWithTypesAndVerb(sTypeNo, oTypeNo, v, column, orderKey, userNo, timestamp) => {
      val oldState = Cache.getAs[String](s"${orderKey} state").getOrElse("paused")
      Cache.set(s"${orderKey} state", "hold")

      implicit val user = User.Empty(userNo)
      var list: JsValue = null
      var kv: JsValue = null
      var single: JsValue = null

      column match {
        case "s" | "o" =>
          val res = Edge.countAllByGroup(sTypeNo, v, oTypeNo, column)
          list = res.foldLeft(Json.arr()){ (a, b: (Long, Long, Long)) =>
            a.append(Json.obj(
              "$subject" -> b._1,
              "value" -> b._2,
              "$object" -> b._3
            ))
          }
          kv = res.foldLeft(Json.arr()){ (a, b: (Long, Long, Long)) =>
            if(column == "s"){
              a.append(Json.obj(
                "key" -> b._1,
                "value" -> b._2
              ))
            }else if(column == "o"){
              a.append(Json.obj(
                "key" -> b._3,
                "value" -> b._2
              ))
            }else{
              a.append(Json.obj(
                "key" -> 0,
                "value" -> b._2
              ))
            }
          }
        case "v" =>
          single = Json.obj("value" -> Edge.countAllByTypesAndVerb(sTypeNo, v, oTypeNo))
      }

      // put RDD or raw data
      if(list != null){
        Cache.set(s"${orderKey} json-list", list.toString )
        // Logger.debug(s"${orderKey} json-list = ${list.toString}")
      }
      if(kv != null){
        Cache.set(s"${orderKey} json-kv", kv.toString )
        // Logger.debug(s"${orderKey} json-kv = ${kv.toString}")
      }
      if(single != null){
        Cache.set(s"${orderKey} json-single", single.toString )
        // Logger.debug(s"${orderKey} json-single = ${single.toString}")
      }

      Cache.set(s"${orderKey} state", oldState)

      
      // edge get, fetch
      // Logger.debug(s"${orderKey} callback pickup = ?")
      Cache.getAs[String](s"${orderKey} callback pickup") match {
        case Some(s: String) => 
          Logger.debug(s"${orderKey} callback pickup = ${s}")
          s.split(',').foreach { pickupNo =>
            Node.findOneByNo(pickupNo.toLong) map { pickup =>
              // pass empty user explicitly
              Actors.pickup ! PickupCallback( Pickup(pickup.toTypedJson), orderKey, User.Empty(userNo))
            } getOrElse {
              // warn
            }
          }
        case None =>
      }
      // it can be there's no pickup for this order.
    }

    case _ => 
      // error
  }
}