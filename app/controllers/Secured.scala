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
import models.{User, Key}

trait Secured {

  // "*" is for full access
  def Signed(scope: String)(f: Request[AnyContent] => User => Result) = Action { implicit request =>
    val apikey = request.queryString.get("apikey").flatMap(_.headOption).getOrElse("")

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
              // signee mismatch
              if(user.no != key.ownerNo){
                Results.Forbidden("key.invalid.owner")
              }else{
                // filter remote address

                // check scope later by Secured.accepted
                if(scope != "*" && !key.scope.contains(scope)){
                  Results.Forbidden("key.invalid.scope")
                }else{
                  // passed
                  f(request)(user)
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