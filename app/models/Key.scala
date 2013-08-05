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

case class Key(var no: Long, var ownerNo: Long, var url: List[String], var address: List[String])

object Key {
  def findByNo(no: Long): Option[Key] = {
    Logger.info("Not implemented")
    None
  }
}