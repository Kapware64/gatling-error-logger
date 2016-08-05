package controllers

import play.api.mvc._
import play.api.i18n._
import play.api.data.Form
import play.api.data.validation.Constraints._
import models._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.Forms._
import dal._

import scala.concurrent.{ExecutionContext}
import javax.inject._

import play.api.libs.json._
import play.api.libs.functional.syntax._

class PersonController @Inject() (repo: PersonRepository, val messagesApi: MessagesApi)
                                 (implicit ec: ExecutionContext) extends Controller with I18nSupport{
  case class PersonEntry(id: Long, name: String, age: Int, gender: String)

  implicit val personReads: Reads[PersonEntry] = (
    (JsPath \ "id").read[Long] and
      (JsPath \ "name").read[String] and
      (JsPath \ "age").read[Int] and
      (JsPath \ "gender").read[String]
    )(PersonEntry.apply _)

  val genderCheckConstraint: Constraint[String] = Constraint("constraints.genderscheck")({
    str =>
      if (str == "M" || str == "F") {
        Valid
      } else {
        Invalid(Seq(ValidationError("You must enter M or F")))
      }
  })

  val validJSON: Constraint[String] = Constraint("constraints.json") { inputjson =>
    try {
      val _ = Json.parse(inputjson).as[List[PersonEntry]]
      Valid
    } catch {
      case e: Exception => Invalid(Seq(ValidationError("Input is not in correct JSON format")))
    }
  }

  /**
   * The mapping for the person form.
   */
  val personForm: Form[CreatePersonForm] = Form {
    mapping(
      "Name" -> nonEmptyText,
      "Age" -> number.verifying(min(0), max(64)),
      "Gender" -> text.verifying(genderCheckConstraint)
    )(CreatePersonForm.apply)(CreatePersonForm.unapply)
  }

  val jsonForm: Form[CreateJsonForm] = Form {
    mapping (
      "JSON" -> text.verifying(validJSON)
    )(CreateJsonForm.apply)(CreateJsonForm.unapply)
  }

  /**
   * The index action.
   */
  def index = Action.async {
    repo.list().map { people =>
      Ok(views.html.index(personForm)(people.length)(jsonForm))
    }
  }

  def descPersons = Action.async { implicit request =>
    repo.list().map { _ =>
      Redirect(routes.PersonController.getPersons())
    }
  }

  def resetAll = Action.async { implicit request =>
    repo.deleteAll().map { _ =>
      Redirect(routes.PersonController.index)
    }
  }

  def constPeople(rawppl: List[PersonEntry]): List[Person] = {
    var ret = List[Person]()
    for (rawperson <- rawppl) {
      ret =  Person(1, rawperson.name, rawperson.age, rawperson.gender) :: ret
    }
    ret
  }

  def inputJSON = Action.async { implicit request =>
    jsonForm.bindFromRequest.fold(
      errorForm => {
        repo.list().map { people =>
          Ok(views.html.index(personForm)(people.length)(errorForm))
        }
      },
      JSON => {
        repo.createmult(constPeople(Json.parse(JSON.json).as[List[PersonEntry]])).map { _ =>
          Redirect(routes.PersonController.index)
        }
      }
    )
  }

  def graphIt = Action.async { implicit request =>
    repo.list().map { people =>
      var retmap:Map[Int, Int] = Map().withDefaultValue(0)
      for ((elem, i) <- people zip List.range(0, 129)) {
        val adjage = if (elem.gender == "M") {elem.age} else {elem.age + 65}
        retmap += (adjage -> (retmap(adjage) + 1))
      }

      Ok(views.html.barchart((0 until 129).map { i => retmap(i) }))
    }
  }

  def addPerson = Action.async { implicit request =>
    personForm.bindFromRequest.fold(
      errorForm => {
        repo.list().map { people =>
          Ok(views.html.index(errorForm)(people.length)(jsonForm))
        }
      },
      person => {
        repo.create(person.name, person.age, person.gender).map { _ =>
          Redirect(routes.PersonController.index)
        }
      }
    )
  }

  def getPersons = Action.async {
  	repo.list().map { people =>
      Ok(Json.toJson(people))
    }
  }
}

case class CreatePersonForm(name: String, age: Int, gender: String)

case class CreateJsonForm(json: String)
