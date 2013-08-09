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

// typeName: error, warn, info, debug
case class Log(var no: Long, var typeName: String, 
	var message: String, var trace: String, var domain: String,
  var remoteAddress: String, var uri: String, var userAgent: String,
  var apikey: String, var scope: String,
  var createdAt: Date = new Date , var updatedAt: Date = new Date ) {

  def toJson: JsObject = {
    Json.obj(
      typeName -> Json.obj(
        "$no" ->        this.no,
        "message" ->    this.message,
        "trace" ->      this.trace,
        "doamin" ->     this.domain,
        "uri" ->        this.uri,
        "remoteAddress" -> this.remoteAddress,
        "userAgent" ->   this.userAgent,
        "apikey" ->   this.apikey,
        "scope" ->   this.scope,
        "$createdAt"  -> this.createdAt,
        "$updatedAt"  -> this.updatedAt
      )
    )
  }

  def save: Option[Long] = {
    val n = Node( this.toJson )
    n.ownerNo = 1
    n.save
  }

  def delete: Boolean = {
    Node.delete(this.no)(User.Default)
  }
}

object Log {

  def apply(json: JsObject): Log = {
    val field = json.fields(0)
    val typeName = field._1
    // val typeNo = Type(typeName).no
    val log = field._2.as[JsObject]
    Log(
      (log \ "$no").as[Long],
      typeName,
      (log \ "message").as[String],
      (log \ "trace").as[String],
      (log \ "domain").as[String],
      (log \ "remoteAddress").as[String],
      (log \ "uri").as[String],
      (log \ "userAgent").as[String],
      (log \ "apikey").as[String],
      (log \ "scope").as[String],
      (log \ "$createdAt").as[Date],
      (log \ "$updatedAt").as[Date]
    )
  }

  // Log.save(1, "read_model", "warn", "exception", ex.stackTrace)
  // User record Log
  def save(userNo: Long, scope: String, typeName: String, message: String, trace: String)(implicit request: RequestHeader) = {
    Log(0, typeName, message, trace, 
      request.domain, request.remoteAddress, 
      request.uri, request.headers("User-Agent"), request.getQueryString("apikey").getOrElse(""),
      scope ).save match {
      case Some(logNo) =>
        Edge( 0L, 1, Node.findOneByNo(userNo).get, "record", Node.findOneByNo(logNo).get).save

      case None =>
    }
  }

}