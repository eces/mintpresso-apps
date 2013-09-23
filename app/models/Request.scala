package models

import play.api.mvc._
import play.api.libs.json._
import play.api.i18n.Messages
import play.api.Play.current
import play.api.Logger
import play.api.libs._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent._
import scala.concurrent.duration._
import java.util.Date

case class Request(var id: String, var no: Long, var uri: String, 
  var threshold: String, var message: String, var trace: String,
  var userAgent: String, var remoteAddress: String, var apiKey: String,
  var stackTrace: String,
  var requestedAt: Date, var acceptedAt: Date, var processedAt: Date,
  var createdAt: Date = new Date , var updatedAt: Date = new Date ) {

  def toJson: JsObject = {
    Json.obj(
      "$id" ->         this.id,
      "$no" ->         this.no,
      "uri" ->         this.uri,
      "threshold" ->   this.threshold,
      "message"   ->   this.message,
      "trace"     ->   this.trace,
      "userAgent"     -> this.userAgent,
      "remoteAddress" -> this.remoteAddress,
      "apiKey"        -> this.apiKey,
      "stackTrace"    -> this.stackTrace,
      "requestedAt" -> this.requestedAt,
      "acceptedAt"  -> this.acceptedAt,
      "processedAt" -> this.processedAt,
      "$createdAt"  -> this.createdAt,
      "$updatedAt"  -> this.updatedAt
    )
  }

  def toTypedJson: JsObject = {
    Json.obj(
      "request" -> this.toJson
    )
  }

  def save: Option[Long] = {
    val k = Node( this.toTypedJson )
    // app@mintpresso.com
    k.ownerNo = 1
    k.save
  }

  def saveAndLog(user: User) = {
    this.save map { requestNo =>
      // save to app@mintpresso.com but belong to each user.
      val edge = Edge( 0L, 1, Node.findOneByNo(user.no).get, "log", Node.findOneByNo(requestNo).get)
      edge.save
      edge.callback(user)
    }
  }

  def delete: Boolean = {
    Node.delete(this.no)(User.Default)
  }
}

object Request {

  def apply(json: JsValue): Request = {
    val key = (json \ "request").as[JsObject]
    Request(
      (key \ "$id").as[String],
      (key \ "$no").as[Long],
      (key \ "uri").as[String],
      (key \ "threshold").as[String],
      (key \ "message").as[String],
      (key \ "trace").as[String],
      (key \ "userAgent").as[String],
      (key \ "remoteAddress").as[String],
      (key \ "apiKey").as[String],
      (key \ "stackTrace").as[String],
      (key \ "requestedAt").as[Date],
      (key \ "acceptedAt").as[Date],
      (key \ "processedAt").as[Date],
      (key \ "$createdAt").as[Date],
      (key \ "$updatedAt").as[Date]
    )
  }

  def findOneByNo(no: Long)(implicit user: User): Option[Request] = {
    Node.findOneByNo(no) map { n =>
      Some(Request(n.toTypedJson))
    } getOrElse{
      None
    }
  }

  def findOneById(id: String)(implicit user: User): Option[Request] = {
    Node.findOneById(id) map { n =>
      Some(Request(n.toTypedJson))
    } getOrElse{
      None
    }
  }

}