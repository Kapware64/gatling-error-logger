package controllers

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Date

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
import org.json4s.DefaultFormats
import org.json4s._
import org.json4s.jackson.Serialization

class ErrorController @Inject() (repo: ErrorRepository, val messagesApi: MessagesApi)
                                (implicit ec: ExecutionContext) extends Controller with I18nSupport{
  case class ErrorList(list: List[Error])
  case class ProcError(name: String, date: Date)
  case class ErrorDates(name: String, dates: List[(Date, Int)])

  implicit val formats = DefaultFormats
  val ADMIN_KEY = ConfigFactory.load()getString("keys.adminKey")

  implicit val errorReads: Reads[Error] = (
    (JsPath \ "id").read[Long] and
      (JsPath \ "name").read[String] and
      (JsPath \ "date").read[String]
    )(Error.apply _)

  val validJSON: Constraint[String] = Constraint("constraints.json") { inputjson =>
    try {
      val _ = Json.parse(inputjson).as[List[Error]]
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

  def constPeople(rawErrors: List[Error]): List[Error] = {
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
        repo.createmult(constPeople(Json.parse(JSON.json).as[List[Error]])).map { _ =>
          repo.list().map { errors =>
            Ok(views.html.index(true)(errors.length)(jsonForm)(passwordForm))
          }
        }.flatMap(identity)
      }
    )
  }

  def graphIt = Action.async { implicit request =>
    repo.list().map { errors =>
      val dateFormat = new SimpleDateFormat("M-d-y")
      def getErrorJson(acc: JValue, rangeAcc: List[JObject], l: List[(ProcError, Int)]): JValue = {
        def addRange(acc: JValue, range: List[JObject]): JValue = {
          val JString(startDate) = range.last \ "name"
          val JString(endDate) = range.head \ "name"
          val childToAdd = JObject(List(JField("name", JString(startDate.toString + " to " + endDate.toString)),
            JField("children", JArray(range))))
          val JArray(children) = acc \ "children"
          acc transformField {
            case JField("children", _) => ("children", JArray(childToAdd :: children))
          }
        }

        if(l.isEmpty) addRange(acc, rangeAcc)
        else {
          if(rangeAcc.length >= 10) getErrorJson(addRange(acc, rangeAcc), List[JObject](), l)
          else getErrorJson(acc, JObject(List(JField("name", JString(dateFormat.format(l.head._1.date))), JField("size", JInt(l.head._2)))) :: rangeAcc, l.tail)
        }
      }

      def procErrors(errors: Seq[Error]): JObject = {
        val nameMatchedErrors = errors.groupBy(_.name).values.toSeq
        val jsonChildren = nameMatchedErrors.foldLeft(List[JValue]()) { (aL, eL) =>
          val dateCounts = eL.foldLeft(List[ProcError]()) { (a, e) =>
            val procDate = dateFormat.parse(e.date)
            a ++ List(ProcError(e.name, procDate))
          }.groupBy(identity).mapValues(_.size).toList.sortBy(_._1.date.getTime)
          getErrorJson(JObject(List(JField("name", JString(eL.head.name)), JField("children", JArray(List[JObject]())))), List[JObject](), dateCounts) :: aL
        }
        JObject(List(JField("name", JString("Failed Requests")), JField("children", JArray(jsonChildren))))
      }

      Ok(views.html.barchart(Serialization.write(procErrors(errors))))
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
