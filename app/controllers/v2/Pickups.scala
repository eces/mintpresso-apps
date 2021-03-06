package controllers.v2

import play.api._
import play.api.mvc._
import play.api.cache._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.Play.current
import controllers._
import models.{Node, Edge, Type, Pickup, User}

object Pickups extends Controller with Secured {

  // request.path.endsWith json or xml
  def findOneById(id: String) = Action { implicit request =>
    Node.findOneByTypeNoAndId(Type("pickup").no, id)(User.Default) match {
      case Some(node) => 
        Edge.findAllByTypeNoAndObject(Type("order").no, "respond", node) match {
          case first :: other => {
            val sNo = first.s.no
            Cache.getAs[String](s"order ${sNo} json") match {
              case Some(json) => Callback(Results.Ok, Json.parse(json))
              case None => Status(429)("pickup.cache.empty") 
            }
          }
          case List() => Status(404)("order.empty") 
        }
      case None =>
        NotFound
    }
  }

  def findOneByNo(no: Long) = Signed("read_pickup") { implicit request => implicit user =>
    Node.findOneByNo(no) match {
      case Some(node) => 
        val pickup = Pickup(node.toTypedJson)
        Cache.getAs[String](s"order ${pickup.orderNo} json-kv") match {
          case Some(json) => Callback(Results.Ok, Json.parse(json))
          
          // skip double check
          // Status(404)("order.empty")
          case None => Status(429)("pickup.cache.empty") 
        }
        // Edge.findAllByTypeNoAndObject(Type("order").no, "respond", node) match {
        //   case first :: other => {
        //     val sNo = first.s.no
        //     Cache.getAs[String](s"order ${sNo} json") match {
        //       case Some(json) => Callback(Results.Ok, Json.parse(json))
        //       case None => Status(429)("pickup.cache.empty") 
        //     }
        //   }
        //   case List() => Status(404)("order.empty") 
        // }
      case None =>
        NotFound
    }
  }

  def prepare(no: Long) = Signed("manage_pickup") { implicit request => implicit user =>
    Node.findOneByNo(no) match {
      case Some(node) => 
        val p = Pickup(node.toTypedJson)
        if(p.prepare(s"order ${p.orderNo}")(user)){
          Accepted
        }else{
          p.cancel
          Ok
        }
      case None =>
        NotFound
    }
  }

  def cancel(no: Long) = Signed("manage_pickup") { implicit request => implicit user =>
    Node.findOneByNo(no) match {
      case Some(node) => 
        if(Pickup(node.toTypedJson).cancel){
          Accepted
        }else{
          Ok
        }
      case None =>
        NotFound
    }
  }

}