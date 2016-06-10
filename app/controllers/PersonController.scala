package controllers

import play.api._
import play.api.mvc._
import play.api.i18n._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.libs.json.Json
import models._
import dal._

import scala.concurrent.{ ExecutionContext, Future }
import scala.collection.mutable.Stack

import javax.inject._

class PersonController @Inject() (repo: PersonRepository, val messagesApi: MessagesApi)
                                 (implicit ec: ExecutionContext) extends Controller with I18nSupport{

  /**
   * The mapping for the person form.
   */
  val personForm: Form[CreatePersonForm] = Form {
    mapping(
      "Name" -> nonEmptyText,
      "Age" -> number.verifying(min(0), max(70)),
      "Gender" -> nonEmptyText
    )(CreatePersonForm.apply)(CreatePersonForm.unapply)
  }

  /**
   * The index action.
   */
  def index = Action.async {
    repo.list().map { people =>
      Ok(views.html.index(personForm)(people.length))
    }
  }

  def barchart = Action.async {
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

  def inputJSON = TODO

  def graphIt = Action.async { implicit request =>
    repo.list().map { _ =>
      Redirect(routes.PersonController.barchart())
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
          Ok(views.html.index(errorForm)(people.length))
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
