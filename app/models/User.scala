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

case class User(var id: String, var no: Long, var password: String, var email: String, var name: String, var phone: String,
  var verified: Boolean = false, var readAt: Date = new Date, var createdAt: Date = new Date ) {

  def toJson: JsObject = {
    Json.obj(
      "$id" ->        this.id,
      "$no" ->        this.no,
      "password" ->   this.password,
      "name" ->       this.name,
      "email" ->      this.email,
      "phone" ->      this.phone,
      "verified" ->   this.verified,
      "readAt" ->     this.readAt.getTime,
      "createdAt" ->  this.createdAt.getTime
    )
  }
}

object User {
  def apply(json: JsValue): User = {
    val user = (json \ "user").as[JsObject]
    User(
      (user \ "$id").as[String],
      (user \ "$no").as[Long],
      (user \ "password").as[String],
      (user \ "email").as[String],
      (user \ "name").as[String],
      (user \ "phone").as[String],
      (user \ "verified").asOpt[Boolean].getOrElse(false),
      (user \ "readAt").asOpt[Date].getOrElse( new Date ),
      (user \ "$createdAt").as[Date]
    )
  }

  def apply(request: RequestHeader): User = {
    User(
      request.session.get("id").getOrElse(""),
      request.session.get("no").getOrElse("0").toLong,
      "",
      request.session.get("email").getOrElse(""),
      request.session.get("name").getOrElse("Unknown"),
      "",
      request.session.get("verified").getOrElse("false").toBoolean,
      new Date( request.session.get("readAt").getOrElse( "0" ).toLong ),
      new Date( request.session.get("createdAt").getOrElse( "0" ).toLong )
    )
  }

  def findOneByNo(no: Long): Option[User] = {
    Logger.info("Not implemented")
    None
  }

  def findOneById(id: String): Option[User] = {
    Logger.info("Not implemented")
    None
  }

  def save(u: User): Boolean = {
    Logger.info("Not implemented")
    true
  }

}