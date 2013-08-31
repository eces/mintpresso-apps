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

  def delete(implicit user: User): Boolean = {
    DB.withConnection { implicit conn =>
      SQL(
"""
DELETE FROM `edges`
WHERE `no` = {no}
  AND `owner` = {ownerNo}
LIMIT 1
"""
      ).on(
        'no -> this.no,
        'ownerNo -> user.no
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
        new Edge(no.get, ownerNo, Node.findOneByNo(s).get, v, Node.findOneByNo(o).get, Json.parse(json).as[JsObject], createdAt, updatedAt)
      }
    }
  }

  def parserWithNodes(s: Node, o: Node) = {
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
      case no~ownerNo~sNo~sT~v~oNo~oT~json~createdAt~updatedAt => {
        new Edge(no.get, ownerNo, s, v, o, Json.parse(json).as[JsObject], createdAt, updatedAt)
      }
    }
  }

  def findOne(sNo: Long, v: String, oNo: Long)(implicit user: User = User.Default): Option[Edge] = {
    if(v.length == 0){
      None
    }else{
      DB.withConnection { implicit conn =>
        SQL(
  """
  SELECT *
  FROM `edges`
  WHERE `owner` = {ownerNo}
    AND `s` = {s}
    AND `o` = {o}
  ORDER BY `updated` DESC
  LIMIT 1
  """
        ).on( 'ownerNo -> user.no, 
              's -> sNo,
              'o -> oNo
        ).singleOpt(parser)
      }
    }
  }

  def findOneWith(s: Node, v: String, o: Node)(implicit user: User = User.Default): Option[Edge] = {
    if(v.length == 0){
      None
    }else{
      DB.withConnection { implicit conn =>
        SQL(
  """
  SELECT *
  FROM `edges`
  WHERE `owner` = {ownerNo}
    AND `s` = {s}
    AND `o` = {o}
  ORDER BY `updated` DESC
  LIMIT 1
  """
        ).on( 'ownerNo -> user.no, 
              's -> s.no,
              'o -> o.no
        ).singleOpt(parserWithNodes(s, o))
      }
    } 
  }

  def findAllBySubjectAndTypeNo(s: Node, v: String, oTypeNo: Long)(implicit user: User = User.Default): List[Edge] = {
    if(v.length == 0){
      List()
    }else{
      DB.withConnection { implicit conn =>
        SQL(
  """
  SELECT *
  FROM `edges`
  WHERE `owner` = {ownerNo}
    AND `s` = {s}
    AND `oType` = {oType}
  """
        ).on( 'ownerNo -> user.no, 
              's -> s.no,
              'oType -> oTypeNo
        ).as(parser *)
      }
    } 
  }

  def findAllByTypeNoAndObject(sTypeNo: Long, v: String, o: Node)(implicit user: User = User.Default): List[Edge] = {
    if(v.length == 0){
      List()
    }else{
      DB.withConnection { implicit conn =>
        SQL(
  """
  SELECT *
  FROM `edges`
  WHERE `owner` = {ownerNo}
    AND `o` = {o}
    AND `sType` = {sType}
  """
        ).on( 'ownerNo -> user.no, 
              'o -> o.no,
              'sType -> sTypeNo
        ).as(parser *)
      }
    } 
  }

  def findAllByTypes(sTypeNo: Long, v: String, oTypeNo: Long, orderBy: String = "", limit: String = "")(implicit user: User = User.Default): List[Edge] = {
    if(v.length == 0){
      List()
    }else{
      DB.withConnection { implicit conn =>
        SQL(
  s"""
  SELECT *
  FROM `edges`
  WHERE `owner` = {ownerNo}
    AND `sType` = {sType}
    AND `v` = {v}
    AND `oType` = {oType}
  ${orderBy}
  ${limit}
  """
        ).on( 'ownerNo -> user.no, 
              'sType -> sTypeNo,
              'v -> v,
              'oType -> oTypeNo
        ).as(parser *)
      }
    }
  }

  def deleteByNo(no: Long)(implicit user: User): Boolean = {
    DB.withConnection { implicit conn =>
      SQL(
"""
DELETE FROM `edges`
WHERE `no` = {no}
  AND `owner` = {ownerNo}
LIMIT 1
"""
      ).on(
        'no -> no,
        'ownerNo -> user.no
      ).execute()
    }
  }

  def deleteAll(user: User): Boolean = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SET SQL_SAFE_UPDATES=0;
DELETE FROM `edges` WHERE `owner` = {ownerNo};
SET SQL_SAFE_UPDATES=1;
"""
      ).on(
        'ownerNo -> user.no
      ).execute()
    }
  }
}