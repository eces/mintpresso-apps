package controllers.v2

import play.api._
import play.api.mvc._
import controllers._

object Edges extends Controller with Secured {
  
  def findAllByTypes(sT: String, v: String, oT: String) = Signed { implicit request => implicit user =>
    Ok
  }

  def findAllBySubjectNo(sT: String, sNo: Long, v: String, oT: String) = Signed { implicit request => implicit user =>
    Ok
  }

  def findAllBySubjectId(sT: String, sId: String, v: String, oT: String) = Signed { implicit request => implicit user =>
    Ok
  }

  def findAllByObjectNo(sT: String, v: String, oT: String, oNo: Long) = Signed { implicit request => implicit user =>
    Ok
  }

  def findAllByObjectId(sT: String, v: String, oT: String, oId: String) = Signed { implicit request => implicit user =>
    Ok
  }

  def findOne(sT: String, s: String, v: String, oT: String, o: String) = Signed { implicit request => implicit user =>
    Ok
  }

  def add(sT: String, s: String, v: String, oT: String, o: String) = Signed { implicit request => implicit user =>
    Ok
  }

  def delete(no: Long) = Signed { implicit request => implicit user =>
    Ok
  }
}