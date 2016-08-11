package dal

import javax.inject.{Inject, Singleton}

import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import models.Error

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ErrorRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import driver.api._

  private class ErrorTable(tag: Tag) extends Table[Error](tag, "errors") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def date = column[String]("date")
    def * = (id, name, date) <> ((Error.apply _).tupled, Error.unapply)
  }

  private val errors = TableQuery[ErrorTable]

  def create(name: String, date: String): Future[Int] = {
    val q = errors += Error(0, name, date)
    db.run(q)
  }

  def createmult(toAdd: List[Error]) = {
    val q = errors ++= toAdd
    db.run(q)
  }

  def deleteAll(): Future[Int] = db.run {
    errors.delete
  }

  def list(): Future[Seq[Error]] = db.run {
    errors.result
  }
}
