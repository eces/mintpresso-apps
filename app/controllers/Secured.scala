package controllers

import play.api._
import play.api.mvc._
import play.api.Logger
import play.api.data.Forms._
import play.api.data._
import play.api.Play.current
import play.api.libs._

import play.api.libs.concurrent._
import java.util.concurrent._
import play.api.libs.json._

import play.api.libs.concurrent.Execution.Implicits._
/** Uncomment the following lines as needed **/
/**
import play.api.libs.iteratee._
import scala.concurrent.stm._
import akka.util.duration._
import play.api.cache._
**/
import models.{User, Key, Edge}

trait Secured {

  // No need to use Callback if there's no content to respond (NotFound, NoContent)
  def Callback(status: Results.Status, json: JsValue)(implicit request: RequestHeader) = {
    request.getQueryString("callback") match {
      case Some(callback) => status(Jsonp(callback, json))
      case None => status(json)
    }
  }

  // "*" is for full access
  def Signed(scope: String)(f: Request[AnyContent] => User => Result) = Action { implicit request =>
    val apikey = request.getQueryString("apikey").getOrElse("")

    // encrypted by application.secret
    if(apikey.length == 0){
      Results.BadRequest("key.empty")
    }else{
      // example "secret__dhn38sd308sdfh308sdfoi"
      val keys = apikey.split("__")
      if(keys.length != 2){
        Results.Forbidden("key.invalid.form")
      }else{
        // should be form of "{user.no} {apikey.no}"
        val values = Crypto.decryptAES(keys(0)).split(' ')
        if(values.length != 2){
          Results.Forbidden("key.invalid.length")
        }else{
          User.findOneByNo( values(0).toLong ) map { implicit user =>
            Key.findOneByNo( values(1).toLong ) map { key =>
              // user issue key ?
              Edge.findOne(user.no, "issue", key.no) match {
                case Some(e) => {
                  // filter remote address
                  if(key.url.contains("*") || key.address.contains( request.remoteAddress ) ){
                    // check scope later by Secured.accepted
                    if(scope != "*" && !key.scope.contains(scope)){
                      Results.Forbidden("key.blocked.scope")
                    }else{
                      // passed
                      f(request)(user)
                    }
                  }else{
                    Results.Forbidden("key.blocked.filter")
                  }
                }
                case None => {
                  Results.Forbidden("key.blocked.owner")
                }
              }
            } getOrElse {
              Results.Forbidden("key.invalid")
            }
          } getOrElse {
            Results.Forbidden("key.invalid.user")
          }
        }
      }
    }
  }
}