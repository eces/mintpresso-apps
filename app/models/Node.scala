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
    json ++ Json.obj(
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

  def save(n: Node): Option[Long] = {
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
        'owner          -> n.ownerNo, 
        'id             -> n.id, 
        'typeNo         -> n.typeNo, 
        'createdAt      -> n.createdAt, 
        'updatedAt      -> n.updatedAt,
        'referencedAt   -> n.referencedAt, 
        'json           -> Json.stringify(n.json)
      ).executeInsert()
    }
  }

  def delete(n: Node): Boolean = {
    Logger.info("Not implemented")
    false
  }
}