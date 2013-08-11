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

case class Write(log: Log)

class LogActor extends Actor {
  def receive = {
    case Write(log: Log) => {
      
    }
  }
}