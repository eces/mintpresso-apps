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

  def callback(implicit user: User) = {
    import play.api.cache._
    import actors._
    
    // verb
    // Logger.debug(s"${user.no} edge v:${this.v} callback order = ?")
    Cache.getAs[String](s"${user.no} edge v:${this.v} callback order") match {
      case Some(s: String) => 
        // Logger.debug(s"${user.no} edge v:${this.v} callback order = ${s}")
        s.split(',').foreach { orderNo =>
          Node.findOneByNo(orderNo.toLong) map { order =>
            Actors.order ! OrderCallback( Order(order.toTypedJson), user)
          } getOrElse {
            // warn
          }
        }
      case None =>
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

  def findOneByNo(no: Long)(implicit user: User = User.Default): Option[Edge] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SELECT *
FROM `edges`
WHERE `owner` = {ownerNo}
  AND `no` = {no}
ORDER BY `updated` DESC
LIMIT 1
"""
      ).on( 'ownerNo -> user.no, 
            'no -> no
      ).singleOpt(parser)
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
    AND `v` = {v}
    AND `o` = {o}
  ORDER BY `updated` DESC
  LIMIT 1
  """
        ).on( 'ownerNo -> user.no, 
              's -> sNo,
              'v -> v,
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
    AND `v` = {v}
    AND `o` = {o}
  ORDER BY `updated` DESC
  LIMIT 1
  """
        ).on( 'ownerNo -> user.no, 
              's -> s.no,
              'v -> v,
              'o -> o.no
        ).singleOpt(parserWithNodes(s, o))
      }
    } 
  }

  def findAllBySubjectAndTypeNo(s: Node, v: String, oTypeNo: Long, orderBy: String = "", limit: String = "")(implicit user: User = User.Default): List[Edge] = {
    if(v.length == 0){
      List()
    }else{
      DB.withConnection { implicit conn =>
        SQL(
  s"""
  SELECT *
  FROM `edges`
  WHERE `owner` = {ownerNo}
    AND `s` = {s}
    AND `v` = {v}
    AND `oType` = {oType}
  ${orderBy}
  ${limit}
  """
        ).on( 'ownerNo -> user.no, 
              's -> s.no,
              'v -> v,
              'oType -> oTypeNo
        ).as(parser *)
      }
    } 
  }

  def findAllByTypeNoAndObject(sTypeNo: Long, v: String, o: Node, orderBy: String = "", limit: String = "")(implicit user: User = User.Default): List[Edge] = {
    if(v.length == 0){
      List()
    }else{
      DB.withConnection { implicit conn =>
        SQL(
  s"""
  SELECT *
  FROM `edges`
  WHERE `owner` = {ownerNo}
    AND `o` = {o}
    AND `v` = {v}
    AND `sType` = {sType}
  ${orderBy}
  ${limit}
  """
        ).on( 'ownerNo -> user.no, 
              'o -> o.no,
              'v -> v,
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

  val dates = List("created", "updated")

  def orderBy(newest: Option[String], oldest: Option[String]): String = {
    var orderBy = ""
    (newest, oldest) match {
      case (Some(a), _) if dates.contains(a) =>
        orderBy = s"ORDER BY `${a}` desc"
      case (None, Some(b)) if dates.contains(b) =>
        orderBy = s"ORDER BY `${b}` asc"
      case (None, None) =>
        orderBy = "ORDER BY `updated` desc"
      case (_, _) =>
        // warn
        orderBy = "ORDER BY `updated` desc"
    }
    orderBy
  }

  def limitBy(offset: Option[Long], limit: Option[Long]): String = {
    var slice = ""
    (offset, limit) match {
      case (Some(a: Long), Some(b: Long)) if (a >= 0 && b >= 1) =>
        slice = s"LIMIT ${a}, ${b}"
      case (Some(a: Long), _) if (a >= 0) =>
        slice = s"LIMIT ${a}, 100"
      case (_, Some(b: Long)) if (b >= 1) =>
        slice = s"LIMIT 0, ${b}"
      case (None, None) =>
        slice = "LIMIT 0, 100"
      case (_, _) =>
        // warn
        slice = "LIMIT 0, 100"
    }
    slice
  }

  def countAllByGroup(sTypeNo: Long, v: String, oTypeNo: Long, column: String)(implicit user: User = User.Default): List[(Long, Long, Long)] = {
    DB.withConnection { implicit conn =>
      SQL(
s"""
SELECT `s`, `o`, COUNT(`no`) as `count`
FROM `edges`
WHERE `sType` = {sType}
  AND `v` = {v}
  AND `oType` = {oType}
  AND `owner` = {ownerNo}
GROUP BY `${column}`
"""
      ).on(
        'sType -> sTypeNo,
        'v -> v,
        'oType -> oTypeNo,
        'ownerNo -> user.no
      ).as( long("edges.s") ~ long("count") ~ long("edges.o") map(flatten) * )
    }
  }

  def countAllByTypesAndVerb(sTypeNo: Long, v: String, oTypeNo: Long)(implicit user: User = User.Default): Long = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SELECT COUNT(`no`) as `count`
FROM `edges`
WHERE `sType` = {sType}
  AND `v` = {v}
  AND `oType` = {oType}
  AND `owner` = {ownerNo}
"""
      ).on(
        'sType -> sTypeNo,
        'v -> v,
        'oType -> oTypeNo,
        'ownerNo -> user.no
      ).as(scalar[Long].single)
    }
  }

  def findVerbsByOwnerNo(user: User): List[String] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SELECT DISTINCT `v`
FROM `edges`
WHERE `owner` = {ownerNo}
"""
      ).on( 'ownerNo -> user.no )().map { row =>
        row[String]("v")
      }.toList
    } 
  }
}