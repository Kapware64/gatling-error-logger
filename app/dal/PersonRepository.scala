package dal

import javax.inject.{ Inject, Singleton }
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile

import models.Person

import scala.concurrent.{ Future, ExecutionContext }

@Singleton
class PersonRepository @Inject() (dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import driver.api._

  private class PeopleTable(tag: Tag) extends Table[Person](tag, "people") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def age = column[Int]("age")
    def gender = column[String]("gender")
    def * = (id, name, age, gender) <> ((Person.apply _).tupled, Person.unapply)
  }

  private val people = TableQuery[PeopleTable]
  
  def create(name: String, age: Int, gender: String): Future[Int] = {
    val q = people += Person(0, name, age, gender)
    db.run(q)
  }

  def createmult(toAdd: List[Person]) = {
    val q = people ++= toAdd
    db.run(q)
  }

  def deleteAll(): Future[Int] = db.run {
    people.delete
  }

  def list(): Future[Seq[Person]] = db.run {
    people.result
  }
}
