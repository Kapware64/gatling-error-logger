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
      // you have access to all the fields in the form here and can
      // write complex logic here
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
      var ret = Seq[Int]()

      for(i <- 0 to 129) {
        val useM = (i <= 64)
        val pplProcess = (a: Int, p: Person) => {
          val bff = if (useM) 0 else 65
          if (p.age == (i - bff) && ((p.gender == "M" && useM) || (p.gender == "F" && !useM))) {
            a + 1
          }
          else {
            a
          }
        }
        ret = ret ++ Seq(people.foldLeft(0)(pplProcess))
      }

      Ok(views.html.barchart(ret))
    }
  }

  /**
   * The add person action.
   *
   * This is asynchronous, since we're invoking the asynchronous methods on PersonRepository.
   */
  def addPerson = Action.async { implicit request =>
    // Bind the form first, then fold the result, passing a function to handle errors, and a function to handle succes.
    personForm.bindFromRequest.fold(
      // The error function. We return the index page with the error form, which will render the errors.
      // We also wrap the result in a successful future, since this action is synchronous, but we're required to return
      // a future because the person creation function returns a future.
      errorForm => {
        repo.list().map { people =>
          Ok(views.html.index(errorForm)(people.length)(jsonForm))
        }
      },
      // There were no errors in the from, so create the person.
      person => {
        repo.create(person.name, person.age, person.gender).map { _ =>
          // If successful, we simply redirect to the index page.
          Redirect(routes.PersonController.index)
        }
      }
    )
  }

  /**
   * A REST endpoint that gets all the people as JSON.
   */
  def getPersons = Action.async {
  	repo.list().map { people =>
      Ok(Json.toJson(people))
    }
  }
}

/**
 * The create person form.
 *
 * Generally for forms, you should define separate objects to your models, since forms very often need to present data
 * in a different way to your models.  In this case, it doesn't make sense to have an id parameter in the form, since
 * that is generated once it's created.
 */
case class CreatePersonForm(name: String, age: Int, gender: String)

case class CreateJsonForm(json: String)
