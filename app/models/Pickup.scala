package models

import play.api.mvc._
import play.api.Play.current
import play.api.db._
import anorm._ 
import anorm.SqlParser._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.i18n.Messages
import play.api.Play.current
import play.api.Logger
import java.util.Date

case class Pickup(var no: Long, title: String,
  var createdAt: Date, var updatedAt: Date, var referencedAt: Date) {

  def toJson: JsObject = {
    Json.obj(
      "$no"         -> this.no,
      "title"       -> this.title,
      "$createdAt"  -> this.createdAt,
      "$updatedAt"  -> this.updatedAt,
      "$referencedAt" -> this.referencedAt
    )
  }

  def save: Option[Long] = {
    val n = Node(Json.obj( "pickup" -> this.toJson ))
    // app@mintpresso.com
    n.ownerNo = 1
    n.save
  }

  def delete: Boolean = {
    Node.delete(this.no)
  }

  def prepare = {
    // load plan

    // match procedure

    // send scheduled message on Akka to PickupActor.

    // change running state of pickup.

    Logger.info("Not implemented")
    None
  }

  def cancel = {
    // send stop command to Actor.
    
    // change running state to paused.

    Logger.info("Not implemented")
    None
  }
}