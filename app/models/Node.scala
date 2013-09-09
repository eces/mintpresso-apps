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
  var json: JsObject = Json.obj(),
  var createdAt: Date = new Date , var updatedAt: Date = new Date , var referencedAt: Date = new Date ) {

  def toJson: JsObject = {
    this.json ++ Json.obj(
      "$id" ->        this.id,
      "$no" ->        this.no,
      "$createdAt" -> this.createdAt,
      "$updatedAt" -> this.updatedAt,
      "$referencedAt" -> this.referencedAt
    )
  }

  def toTypedJson: JsObject = {
    Json.obj(
     this.typeName -> this.toJson
    )
  }

  // update if Node.no is 0 otherwise insert newly
  def save: Option[Long] = {
    if(this.no == 0L){
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
          'ownerNo        -> this.ownerNo, 
          'id             -> this.id, 
          'typeNo         -> this.typeNo, 
          'createdAt      -> this.createdAt, 
          'updatedAt      -> this.updatedAt,
          'referencedAt   -> this.referencedAt, 
          'json           -> Json.stringify(this.json)
        ).executeInsert()
      }
    }else{
      this.updatedAt = new Date
      DB.withConnection { implicit conn =>
        SQL(
"""
UPDATE `nodes`
SET `owner` = {ownerNo},
    `id` = {id},
    `type` = {typeNo},
    `created` = {createdAt},
    `updated` = {updatedAt},
    `referenced` = {referencedAt},
    `json` = {json}
WHERE `no` = {no}
LIMIT 1
"""
        ).on(
          'ownerNo        -> this.ownerNo, 
          'id             -> this.id, 
          'typeNo         -> this.typeNo, 
          'createdAt      -> this.createdAt, 
          'updatedAt      -> this.updatedAt,
          'referencedAt   -> this.referencedAt, 
          'json           -> Json.stringify(this.json),
          'no             -> this.no
        ).executeUpdate() match {
          case 1 => Some(this.no)
          case 0 => None
        }
      }
    }
  }

  def saveWithNo: Option[Long] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
INSERT INTO `nodes`
(`no`, `owner`, `id`, `type`, `created`, `updated`, `referenced`, `json`)
VALUES (
  {no}, {ownerNo}, {id}, {typeNo}, {createdAt}, {updatedAt}, {referencedAt}, {json}
)
"""
      ).on(
        'no             -> this.no,
        'ownerNo        -> this.ownerNo, 
        'id             -> this.id, 
        'typeNo         -> this.typeNo, 
        'createdAt      -> this.createdAt, 
        'updatedAt      -> this.updatedAt,
        'referencedAt   -> this.referencedAt, 
        'json           -> Json.stringify(this.json)
      ).executeInsert()
    }
  }

  def delete(implicit user: User): Boolean = {
    DB.withConnection { implicit conn =>
      SQL(
"""
DELETE FROM `nodes`
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
    
    // node typeNo:?
    // Logger.debug(s"${user.no} node typeNo:${this.typeNo} callback order = ?")
    Cache.getAs[String](s"${user.no} node typeNo:${this.typeNo} callback order") match {
      case Some(s: String) => 
        // Logger.debug(s"${user.no} node typeNo:${this.typeNo} callback order = ${s}")
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

object Node {
  val parser = {
    get[Pk[Long]]("nodes.no")~
    get[Long]("owner")~ 
    get[String]("id")~
    get[Long]("types.no")~
    get[String]("types.name")~
    get[Date]("created")~ 
    get[Date]("updated")~ 
    get[Date]("referenced")~ 
    get[String]("json") map {
      case no~ownerNo~id~typeNo~typeName~createdAt~updatedAt~referencedAt~j => {
        new Node(id, no.get, ownerNo, typeNo, typeName, Json.parse(j).as[JsObject], createdAt, updatedAt, referencedAt)
      }
    }
  }

  // json from API client
  def apply(json: JsObject): Node = {
    val field = json.fields(0)
    val typeName = field._1
    val typeNo = Type(typeName).no
    var n = field._2.as[JsObject]
    val id = (n \ "$id").asOpt[String].getOrElse("")
    val no = (n \ "$no").asOpt[Long].getOrElse(0L)
    val d = new Date

    // exclude $ reserved keywords
    n -= "$id"
    n -= "$no"
    n -= "$createdAt"
    n -= "$updatedAt"
    n -= "$referencedAt"

    Node(id, no, 0L, typeNo, typeName, n, d, d, d)
  }

  def Empty(no: Long, typeNo: Long) = {
    val d = new Date
    new Node("", no, 0, typeNo, Type(typeNo).name, Json.obj(), d, d, d)
  }

  def findOneByNo(no: Long)(implicit user: User = User.Default): Option[Node] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SELECT *
FROM `nodes`, `types`
WHERE `nodes`.`no` = {no}
  AND `owner` = {ownerNo}
  AND `nodes`.`type` = `types`.`no`
ORDER BY `updated` DESC
LIMIT 1
"""
      ).on( 'no -> no, 
            'ownerNo -> user.no
      ).singleOpt(parser)
    }
  }

  def findOneById(id: String)(implicit user: User = User.Default): Option[Node] = {
    if(id.length == 0){
      None
    }else{
      DB.withConnection { implicit conn =>
        SQL(
  """
  SELECT *
  FROM `nodes`, `types`
  WHERE `id` = {id}
    AND `owner` = {ownerNo}
    AND `nodes`.`type` = `types`.`no`
  ORDER BY `updated` DESC
  LIMIT 1
  """
        ).on( 'id -> id, 
              'ownerNo -> user.no
        ).singleOpt(parser)
      }
    }
  }

  def findOneByTypeNoAndId(typeNo: Long, id: String)(implicit user: User): Option[Node] = {
    DB.withConnection { implicit conn =>
      SQL(
  """
  SELECT *
  FROM `nodes`, `types`
  WHERE `id` = {id}
    AND `owner` = {ownerNo}
    AND `nodes`.`type` = {typeNo}
    AND `types`.`no` = {typeNo}
  ORDER BY `updated` DESC
  LIMIT 1
  """
      ).on( 'id -> id, 
            'ownerNo -> user.no,
            'typeNo -> typeNo
      ).singleOpt(parser)
    }
  }

  // internal use only !
  def findAllByTypeNo(typeNo: Long)(implicit user: User): List[Node] = {
    DB.withConnection { implicit conn =>
      SQL(
  """
  SELECT *
  FROM `nodes`, `types`
  WHERE `owner` = {ownerNo}
    AND `nodes`.`type` = {typeNo}
    AND `types`.`no` = {typeNo}
  ORDER BY `updated` DESC
  """
      ).on( 'ownerNo -> user.no,
            'typeNo -> typeNo
      ).as(parser *)
    }
  }

  // find nodes with full text search in json string.
  // no $keyword is allowed, multiple keys are support, partial match (LIKE) doesn't support.
  // it has and/or issue.
  def findAllByTypeNoAndJson(typeNo: Long, json: String, orderBy: String = "", limit: String = "")(implicit user: User): List[Node] = {
    DB.withConnection { implicit conn =>
      SQL(
  s"""
  SELECT *
  FROM `nodes`, `types`
  WHERE `json` LIKE {json}
    AND `owner` = {ownerNo}
    AND `nodes`.`type` = {typeNo}
    AND `types`.`no` = {typeNo}
  ${orderBy}
  ${limit}
  """
      ).on( 'json -> json, 
            'ownerNo -> user.no,
            'typeNo -> typeNo
      ).as(parser *)
    }
  }

  def delete(no: Long)(implicit user: User): Boolean = {
    DB.withConnection { implicit conn =>
      SQL(
  """
  DELETE FROM `nodes`
  WHERE `no` = {no}
    AND `owner` = {ownerNo}
  LIMIT 1;
  """
      ).on(
        'no -> no,
        'ownerNo -> user.no
      ).execute()
      
      SQL(
  """
  DELETE FROM `edges`
  WHERE (`s` = {no} OR `o` = {no})
    AND `owner` = {ownerNo};
  """
      ).on(
        'no -> no,
        'ownerNo -> user.no
      ).execute()
    }
  }

  def countAllByTypeNo(typeNo: Long)(implicit user: User): Int = {
    DB.withConnection { implicit conn =>
      SQL(
  """
  SELECT COUNT(`no`) as `count`
  FROM `nodes`
  WHERE `type` = {type}
    AND `owner` = {ownerNo}
  """
      ).on(
        'type -> typeNo,
        'ownerNo -> user.no
      ).apply().head[Int]("count")
    }
  }

  def deleteAll(user: User): Boolean = {
    DB.withConnection { implicit conn =>
      SQL(
  """
  SET SQL_SAFE_UPDATES=0;
  DELETE FROM `nodes` WHERE `owner` = {ownerNo};
  SET SQL_SAFE_UPDATES=1;
  """
      ).on(
        'ownerNo -> user.no
      ).execute()
    }
  }

  val dates = List("created", "updated", "referenced")

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

  def findTypesByOwnerNo(user: User): List[(Long, String)] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SELECT DISTINCT `types`.`no`, `types`.`name`
FROM `nodes`, `types`
WHERE `nodes`.`owner` = {ownerNo}
  AND `nodes`.`type` = `types`.`no`
"""
      ).on( 'ownerNo -> user.no )().map { row =>
        row[Long]("types.no") -> row[String]("types.name")
      }.toList
    } 
  }
}