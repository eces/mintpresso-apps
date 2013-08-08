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

case class Edge(var no: Long, var ownerNo: Long, var s: Node, var v: String, var o: Node,
  var json: JsObject = Json.obj(), var createdAt: Date = new Date , var updatedAt: Date = new Date ) {

  def toJson: JsObject = {
    this.json ++ Json.obj(
      "$no" ->        this.no,
      "$subject" ->   this.s.toJson,
      "$verb" ->      this.v,
      "$object" ->    this.o.toJson,
      "$createdAt" -> this.createdAt,
      "$updatedAt" -> this.updatedAt
    )
  }

  def save: Option[Long] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
INSERT INTO `edges`
(`owner`, `s`, `sType`, `v`, `o`, `oType`, `json`, `created`, `updated`)
VALUES (
  {ownerNo}, {sNo}, {sTypeNo}, {v}, {oNo}, {oTypeNo}, {json}, {createdAt}, {updatedAt}
)
"""
      ).on(
        'ownerNo        -> this.ownerNo, 
        'sNo            -> this.s.no, 
        'sTypeNo        -> this.s.typeNo, 
        'v              -> this.v,
        'oNo            -> this.o.no, 
        'oTypeNo        -> this.o.typeNo, 
        'json           -> Json.stringify(this.json),
        'createdAt      -> this.createdAt, 
        'updatedAt      -> this.updatedAt
      ).executeInsert()
    }
  }

  def delete: Boolean = {
    DB.withConnection { implicit conn =>
      SQL(
"""
DELETE FROM `edges`
WHER `no` = {no}
LIMIT 1
"""
      ).on(
        'no -> this.no
      ).execute()
    }
  }
}

object Edge {
  val parser = {
    get[Pk[Long]]("no")~
    get[Long]("owner")~ 
    get[Long]("s")~
    get[Long]("sType")~
    get[String]("v")~
    get[Long]("o")~
    get[Long]("oType")~
    get[String]("json")~
    get[Date]("created")~ 
    get[Date]("updated") map {
      case no~ownerNo~s~sT~v~o~oT~json~createdAt~updatedAt => {
        new Edge(no.get, ownerNo, Node.empty(s, sT), v, Node.empty(o, oT), Json.parse(json).as[JsObject], createdAt, updatedAt)
      }
    }
  }
}