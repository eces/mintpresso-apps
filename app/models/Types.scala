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


case class Type (var no: Long, var name: String) {
  // field `name` is unique
  def save: Option[Long] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
INSERT IGNORE INTO `types` (`name`)
VALUES ( {name} )
"""
      ).on( 'name -> this.name ).executeInsert()
    }
  }
}

object Type {
  val parser = {
    get[Pk[Long]]("no")~
    get[String]("name") map {
      case no~name => {
        new Type(no.get, name)
      }
    }
  }

  def apply(no: Long) = {
    Type.findOneByNo(no)
  }

  def apply(name: String) = {
    Type.findOneByName(name) map { t =>
      t
    } getOrElse {
      val t = new Type(0L, name)
      t.save match {
        case Some(no: Long) => Type.findOneByNo(no)
        case None => throw new Exception("Type not created.")
      }
    }
    
  }

  def findOneByNo(no: Long): Type = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SELECT *
FROM `types`
WHERE `no` = {no}
"""
      ).on( 'no -> no ).as(parser.single)
    }
  }

  // create if it doesn't exist
  def findOneByName(name: String): Option[Type] = {
    DB.withConnection { implicit conn =>
      SQL(
"""
SELECT *
FROM `types`
WHERE `name` = {name}
"""
      ).on( 'name -> name ).as(parser.singleOpt)
    }
  }
}