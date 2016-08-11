package controllers

import play.api.mvc._
import play.api.i18n._
import play.api.data.Form
import models.{Error, _}
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.data.Forms._
import dal.{ErrorRepository, _}

import scala.concurrent.ExecutionContext
import javax.inject._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import com.typesafe.config.ConfigFactory

class ErrorController @Inject() (repo: ErrorRepository, val messagesApi: MessagesApi)
                                (implicit ec: ExecutionContext) extends Controller with I18nSupport{
  case class ErrorEntry(id: Long, name: String, date: String)
  val ADMIN_KEY = ConfigFactory.load()getString("keys.adminKey")

  implicit val errorReads: Reads[ErrorEntry] = (
    (JsPath \ "id").read[Long] and
      (JsPath \ "name").read[String] and
      (JsPath \ "date").read[String]
    )(ErrorEntry.apply _)

  val validJSON: Constraint[String] = Constraint("constraints.json") { inputjson =>
    try {
      val _ = Json.parse(inputjson).as[List[ErrorEntry]]
      Valid
    } catch {
      case e: Exception => Invalid(Seq(ValidationError("Input is not in correct JSON format")))
    }
  }

  val jsonForm: Form[CreateJsonForm] = Form {
    mapping (
      "JSON" -> text.verifying(validJSON)
    )(CreateJsonForm.apply)(CreateJsonForm.unapply)
  }

  val validPassword: Constraint[String] = Constraint("constraints.json") { enteredPassword =>
    if(enteredPassword == ADMIN_KEY) Valid
    else Invalid(Seq(ValidationError("Incorrect password!")))
  }

  val passwordForm: Form[CreatePasswordForm] = Form {
    mapping (
      "Password" -> text.verifying(validPassword)
    )(CreatePasswordForm.apply)(CreatePasswordForm.unapply)
  }

  def index = Action.async {
    repo.list().map { errors =>
      Ok(views.html.index(false)(errors.length)(jsonForm)(passwordForm))
    }
  }

  def reqAdmin = Action.async { implicit request =>
    passwordForm.bindFromRequest.fold(
      errorForm => {
        repo.list().map { errors =>
          Ok(views.html.index(false)(errors.length)(jsonForm)(errorForm))
        }
      },
      Password => {
        repo.list().map { errors =>
          Ok(views.html.index(true)(errors.length)(jsonForm)(passwordForm))
        }
      }
    )
  }

  def descErrors = Action.async { implicit request =>
    repo.list().map { _ =>
      Redirect(routes.ErrorController.getErrors())
    }
  }

  def resetAll = Action.async { implicit request =>
    repo.deleteAll().map { _ =>
      Ok(views.html.index(true)(0)(jsonForm)(passwordForm))
    }
  }

  def constPeople(rawErrors: List[ErrorEntry]): List[Error] = {
    var ret = List[Error]()
    for (rawError <- rawErrors) {
      ret = Error(1, rawError.name, rawError.date) :: ret
    }
    ret
  }

  def inputJSON = Action.async { implicit request =>
    jsonForm.bindFromRequest.fold(
      errorForm => {
        repo.list().map { errors =>
          Ok(views.html.index(true)(errors.length)(errorForm)(passwordForm))
        }
      },
      JSON => {
        repo.createmult(constPeople(Json.parse(JSON.json).as[List[ErrorEntry]])).map { _ =>
          repo.list().map { errors =>
            Ok(views.html.index(true)(errors.length)(jsonForm)(passwordForm))
          }
        }.flatMap(identity)
      }
    )
  }

  def graphIt = Action.async { implicit request =>
    repo.list().map { errors =>
      Ok(views.html.barchart((0 until 129).map { i => 0 }))
    }
  }

  def addError = Action { request =>
    request.body.asJson.map { json =>
      val JsDefined(JsString(key)) = json \ "key"
      val JsDefined(content) = json \ "content"

      if(key.toString != ADMIN_KEY) {
        BadRequest("Verification failed")
      }
      else {
        content.asOpt[Seq[JsObject]].map { errors =>
          def constructErrors(acc: List[Error], l: Seq[JsObject]): List[Error] = {
            if(l.isEmpty) acc
            else {
              val hd = l.head
              val JsDefined(name) = hd \ "name"
              val JsDefined(date) = hd \ "date"
              constructErrors(Error(0, name.toString.replace("\"", ""), date.toString.replace("\"", "")) :: acc, l.tail)
            }
          }

          repo.createmult(constructErrors(List[Error](), errors)).map { _ =>
            Redirect(routes.ErrorController.index)
          }
          Ok("Success")
        }
      }.getOrElse {
        BadRequest("Incorrect json format")
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }

  def getErrors = Action.async {
    repo.list().map { errors =>
      Ok(Json.toJson(errors))
    }
  }
}

case class CreatePasswordForm(password: String)
case class CreateJsonForm(json: String)
