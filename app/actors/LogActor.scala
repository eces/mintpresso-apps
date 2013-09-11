package actors

import play.api._
import play.api.libs._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.i18n.Messages
import akka.actor.{Actor, Props, ActorSystem}
import akka.actor.Actor._
import scala.concurrent._
import scala.concurrent.duration._
import models._

case class Error(message: String, log: Request, user: User)
case class Warn(message: String, log: Request, user: User)
case class Info(message: String, log: Request, user: User)
case class Debug(message: String, log: Request, user: User)

class LogActor extends Actor {
  def receive = {
    case Error(message: String, log: Request, user: User) => {
      log.threshold = "error"
      log.saveAndLog(user)
    }
    case Warn(message: String, log: Request, user: User) => {
      log.threshold = "warn"
      log.saveAndLog(user)
    }
    case Info(message: String, log: Request, user: User) => {
      log.threshold = "info"
      log.saveAndLog(user)
    }
    case Debug(message: String, log: Request, user: User) => {
      // Logger.info("R> " + (log.acceptedAt.getTime - log.requestedAt.getTime))
      // Logger.info("P> " + (log.processedAt.getTime - log.acceptedAt.getTime))
      log.threshold = "debug"
      log.saveAndLog(user)
    }
  }
}