package test

import org.specs2.mutable._
import org.specs2.matcher.MatchResult

import play.api.test._
import play.api.test.Helpers._
import play.api._
import play.api.mvc._
import play.api.i18n.Messages
import play.api.data.Forms._
import play.api.data._
import play.api.Play.current
import play.api.libs._
import play.api.libs.ws._
import play.api.cache._
import play.api.libs.json._
import play.api.libs.iteratee._
import scala.concurrent.stm._
import scala.concurrent._
import scala.concurrent.duration._

import java.util.Date

class APISpec extends Specification {

  "API" should {

    "block empty key" in {
      true
    }

    "block an invalid form of key" in {
      true
    }

    "block invalid key length" in {
      true
    }

    "block invalid owner with given key" in {
      true
    }

    "block the action out of its scope" in {
      true
    }

    "block key not found" in {
      true
    }

    "block user not found" in {
      true
    }


  }

  // "Node API" should {
  //   "create new type as needed" in {
  //     true
  //   }
  //   "not create new type if it's already exist" in {
  //     true
  //   }
  // }
}
