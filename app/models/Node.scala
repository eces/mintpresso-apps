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

case class Node(var id: String, var no: Long, var ownerNo: Long, var typeNo: Long, var typeName: String,
  var json: JsObject, var createdAt: Date, var updatedAt: Date, var referencedAt: Date) {

  def toJson: JsObject = {
    this.json ++ Json.obj(
      "$id" ->        this.id,
      "$no" ->        this.no,
      "$createdAt" -> this.createdAt,
      "$updatedAt" -> this.updatedAt,
      "$referencedAt" -> this.referencedAt
    )
  }

  // def toTypedJson: JsObject = {
  //  Json.obj(
  //    this.typeName -> this.toJson
  //  )
  // }

  def save: Option[Long] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
INSERT INTO `nodes`
(`owner`, `id`, `type`, `created`, `updated`, `referenced`, `json`)
VALUES (
  {ownerNo}, {id}, {typeNo}, {createdAt}, {updatedAt}, {referencedAt}, {json}
)
"""
      ).on(
        'owner          -> this.ownerNo, 
        'id             -> this.id, 
        'typeNo         -> this.typeNo, 
        'createdAt      -> this.createdAt, 
        'updatedAt      -> this.updatedAt,
        'referencedAt   -> this.referencedAt, 
        'json           -> Json.stringify(this.json)
      ).executeInsert()
    }
  }

  def delete: Boolean = {
    DB.withConnection { implicit conn =>
      SQL(
"""
DELETE FROM `nodes`
WHER `no` = {no}
LIMIT 1
"""
      ).on(
        'no -> this.no
      ).execute()
    }
  }
}

object Node {
  val parser = {
    get[Pk[Long]]("no")~
    get[Long]("owner")~ 
    get[String]("id")~
    get[Long]("typeNo")~
    get[String]("typeName")~
    get[Date]("created")~ 
    get[Date]("updated")~ 
    get[Date]("referenced")~ 
    get[String]("json") map {
      case no~ownerNo~id~typeNo~typeName~createdAt~updatedAt~referencedAt~json => {
        new Node(id, no.get, ownerNo, typeNo, typeName, Json.parse(json).as[JsObject], createdAt, updatedAt, referencedAt)
      }
    }
  }

  // json from API client
  def apply(json: JsObject): Node = {
    val field = json.fields(0)
    val typeName = field._1
    val typeNo = Type(typeName).no
    val n = field._2.as[JsObject]
    val id = (n \ "$id").asOpt[String].getOrElse("")
    val no = (n \ "$no").asOpt[Long].getOrElse(0L)
    val d = new Date

    // exclude $ reserved keywords
    n - "$id" - "$no" - "$createdAt" - "$updatedAt" - "$referencedAt"

    Node(id, no, 0L, typeNo, typeName, n, d, d, d)
  }

  def empty(no: Long, typeNo: Long) = {
    val d = new Date
    new Node("", no, 0, typeNo, Type(typeNo).name, Json.obj(), d, d, d)
  }

  def findOneByNo(no: Long)(implicit user: User): Option[Node] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SELECT `owner`, `type` as `typeNo`, `name` as `typeName`, `id`, `created`, `updated`, `referenced`, `json`,
FROM `nodes` and `types`
WHERE `nodes`.`no` = {no}
  AND `owner` = {ownerNo}
  AND `nodes`.`type` = `types`.`no`
"""
      ).on( 'no -> no, 
            'user -> user.no
      ).singleOpt(parser)
    }
  }

  def findOneById(id: String)(implicit user: User): Option[Node] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SELECT `owner`, `type` as `typeNo`, `name` as `typeName`, `id`, `created`, `updated`, `referenced`, `json`,
FROM `nodes` and `types`
WHERE `id` = {id}
  AND `owner` = {ownerNo}
  AND `nodes`.`type` = `types`.`no`
"""
      ).on( 'id -> id, 
            'user -> user.no
      ).singleOpt(parser)
    }
  }

  // find nodes with full text search in json string.
  // no $keyword is allowed, multiple keys are support, partial match (LIKE) doesn't support.
  // it has and/or issue.
  def findAllByJson(json: JsObject) = {
    Logger.info("Not implemented")
    None
  }

  def delete(no: Long): Boolean = {
    DB.withConnection { implicit conn =>
      SQL(
"""
DELETE FROM `nodes`
WHER `no` = {no}
LIMIT 1
"""
      ).on(
        'no -> no
      ).execute()
    }
  }
}