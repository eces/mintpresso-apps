package actors

import play.api._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import akka.actor.{Actor, Props, ActorSystem}
import akka.actor.Actor._

object Actors {
  val log = Akka.system.actorOf(Props[LogActor], name = "LogActor")
  val order = Akka.system.actorOf(Props[OrderActor], name = "OrderActor")
  val pickup = Akka.system.actorOf(Props[PickupActor], name = "PickupActor")
}