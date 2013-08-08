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
  var verified: Boolean = false, var testmode: Boolean = true,
  var orderLimit: Int = 5, var rateLimit: Int = 50000, var rateRemaining: Int = 50000,
  var readAt: Date = new Date, var createdAt: Date = new Date ) {

  def toJson: JsObject = {
    Json.obj(
      "$id" ->        this.id,
      "$no" ->        this.no,
      "password" ->   this.password,
      "name" ->       this.name,
      "email" ->      this.email,
      "phone" ->      this.phone,
      "verified" ->   this.verified,
      "testmode" ->   this.testmode,
      "orderLimit" -> this.orderLimit,
      "rateLimit" ->  this.rateLimit,
      "rateRemaining" -> this.rateRemaining,
      "readAt" ->     this.readAt.getTime,
      "createdAt" ->  this.createdAt.getTime
    )
  }

  def toTypedJson: JsObject = {
   Json.obj(
     "user" -> this.toJson
   )
  }

  def save: Boolean = {
    Logger.info("Not implemented")
    true
  }

}

object User {
  val Default = User("app@mintpresso.com", 1L, "", "app@mintpresso.com", "mintpresso", "", true, false)

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
      (user \ "testmode").as[Boolean],
      (user \ "orderLimit").as[Int],
      (user \ "rateLimit").as[Int],
      (user \ "rateRemaining").as[Int],
      (user \ "readAt").asOpt[Date].getOrElse( new Date ),
      (user \ "$createdAt").as[Date]
    )
  }

  def findOneByNo(no: Long)(implicit user: User = User.Default): Option[User] = {
    Node.findOneByNo(no) map { n =>
      Some(User(n.toJson))
    } getOrElse{
      None
    }
  }

  def findOneById(id: String)(implicit user: User = User.Default): Option[User] = {
    Node.findOneById(id) map { n =>
      Some(User(n.toJson))
    } getOrElse{
      None
    }
  }

}