import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.concurrent._
import play.api.Play.current

object Global extends GlobalSettings {
  
  val APIFilter = Filter { (next, rh) =>
    val d = new java.util.Date
    rh.path match {
      case p if p == "/" => next(rh)
      case p if p.endsWith(".json") => {
        next(rh).withHeaders (
          "Content-Type" -> "application/json; charset=utf-8",
          "Cache-Control" -> "max-age=86400",
          "X-Content-Type-Options" -> "nosniff"
        )
      }
      case p if p.endsWith(".xml") => {
        next(rh).withHeaders (
          "Content-Type" -> "application/xml; charset=utf-8",
          "Cache-Control" -> "max-age=86400",
          "X-Content-Type-Options" -> "nosniff"
        )
      }
      case _ => {
        next(rh).withHeaders (
          "Date" -> d.getTime.toString,
          "Content-Type" -> "application/json; charset=utf-8",
          "Cache-Control" -> "max-age=0, private, must-revalidate",
          "X-Content-Type-Options" -> "nosniff"
        )
      }
    }
  }

  override def doFilter(action: EssentialAction) = {
    APIFilter(action)
  }

  override def onStart(app: Application) {
    import models._
    import play.api.libs._
    import play.api.libs.json._
    import java.util.Date
    Node.findOneByNo(1) match {
      case Some(n) => {}
      case None => {
        val user = User.Default
        user.password = "reset"
        val node1 = Node( user.toTypedJson )
        node1.no = 1
        node1.ownerNo = 1
        node1.saveWithNo match {
          case Some(no) => {
            user.no = no
            Logger.info(s"user(app@mintpresso.com) created.")
          }
          case None => {
            Logger.error(s"user(app@mintpresso.com) not created.")
          }
        }

      val key = new Key("", 0L, List("*"), List(""), List("read_model", "search_model", "create_model", "update_model", "delete_model", "search_status", "create_status", "delete_status", "manage_order", "manage_pickup", "read_type", "read_verb"))
        val node2 = Node( key.toTypedJson )
        node2.no = 2
        node2.ownerNo = 1
        node2.saveWithNo match {
          case Some(keyNo) => {
            key.no = keyNo
            key.id = "secret__" + Crypto.encryptAES(user.no + " " + key.no)
            key.save
            Edge( 0L, 1, Node.findOneByNo(user.no).get, "issue", Node.findOneByNo(key.no).get).save
            Logger.info(s"key(${key.id}) created and issued.")
          }
          case None =>
            Logger.info(s"key(...) not created.")
        }

        // user log request - count
        val o1 = Order(0, "user log request - count", "paused", "status", "user log request",
          List(Json.obj("key" -> "count", "value" -> "s")), "500 ms", new Date , new Date )
        o1.save
        Logger.info(s"order(user log request - count) created.")

        // user logs request - add
        val p1 = Pickup(0, "user logs request - add", "paused", "status", "user logs request",
          List(Json.obj("key" -> "add", "value" -> "")), 3, new Date , new Date )
        p1.save
        Logger.info(s"pickup(user logs request - add) created.")

      }
    }

    import actors._
    Edge.findAllByTypes(Type("user").no, "request", Type("order").no).foreach { orderReq =>
      val user = User(orderReq.s.toTypedJson)
      val order = Order(orderReq.o.toTypedJson)
      val orderKey = s"order ${order.no}"
      if(order.state != "paused"){
        Edge.findAllBySubjectAndTypeNo(orderReq.o, "callback", Type("pickup").no).foreach { pickupReq =>
          val pickup = Pickup(pickupReq.o.toTypedJson)
          if(pickup.state != "paused"){
            // prepare pickups
            Actors.pickup ! PickupCallback(pickup, orderKey, user)
          }
        }
        // prepare orders
        Actors.order ! OrderCallback(order, user)
      }
    }
  }  
  
  override def onStop(app: Application) {

  }

  override def onError(request: RequestHeader, ex: Throwable) = {
    Logger.warn(s"[Error] ${request}")
    Logger.warn(s"[Error] ${ex}")
    Results.InternalServerError
  }

  override def onHandlerNotFound(request: RequestHeader): Result = {
    Logger.warn(s"[HandlerNotFound] ${request}")
    Results.NotFound("unknown.api")
  }

  override def onBadRequest(request: RequestHeader, error: String) = {
    Logger.warn(s"[BadRequest] ${request}")
    Logger.warn(s"[BadRequest] ${error}")
    Results.BadRequest
  }
    
}