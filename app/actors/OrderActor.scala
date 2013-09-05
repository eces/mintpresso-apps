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
// case class RowCount
// case class Rank
// case class Clean
// case class FrequentRelationCount
// case class AR
// case class CF

class OrderActor extends Actor {
  def receive = {

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
      var json: JsValue = null

      column match {
        case "s" | "o" =>
          json = Edge.countAllByGroup(sTypeNo, v, oTypeNo, column).foldLeft(Json.arr()){ (a, b: (Long, Long)) =>
            a.append(Json.obj(
              "key" -> b._1,
              "value" -> b._2
            ))
          }
        case "v" =>
          json = Json.obj( "value" -> Edge.countAllByTypesAndVerb(sTypeNo, v, oTypeNo) )
      }

      // put RDD or raw data
      // Cache.set(s"${orderKey} raw", count)
      Cache.set(s"${orderKey} json", json.toString )
      Cache.set(s"${orderKey} state", oldState)

      Logger.debug(s"${orderKey} json = ${json.toString}")
      
      Cache.getAs[Long](s"${orderKey} callback pickup") map { nodeNo =>
        Node.findOneByNo(nodeNo) match {
          case Some(node) => 
            // pass empty user implicitly
            Pickup(node.toTypedJson).prepare(orderKey)(User.Empty(userNo))
          case None =>
            // warn
        }
      }
      // it can be there's no pickup for this order.
    }

    case _ => 
      // error
  }
}