package wuebutab

import scala.util.{Try, Success, Failure}
import scala.jdk.CollectionConverters._

import com.google.api.services.sheets.v4._
import com.google.api.services.sheets.v4.model._

import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.googleapis.json.GoogleJsonResponseException
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport

class SpreadsheetHandle(
  val applicationName: String,
  val credentialsPath: String,
  val tokensPath: String,
  val spreadsheetId: String):

  // Internal

  private val transport = GoogleNetHttpTransport.newTrustedTransport

  private val auth = Auth(credentialsPath, tokensPath, transport)

  private val service = 
    new Sheets.Builder(transport, GsonFactory.getDefaultInstance(), auth.credential)
      .setApplicationName(applicationName)
      .build

  private def batchRequest(requests: Seq[Request]): Sheets#Spreadsheets#BatchUpdate =
    service.spreadsheets.batchUpdate(
      spreadsheetId, 
      BatchUpdateSpreadsheetRequest().setRequests(requests.toList.asJava))

  // Interface

  def getTitle: String =
    val request = service.spreadsheets.get(spreadsheetId)
    val response = request.execute()
    response.getProperties.getTitle

  def readRange(range: String): Vector[Vector[String]] =
    val request = service.spreadsheets.values.get(spreadsheetId, range)
    val response = request.execute()
    response.getValues.asNestedSeq.map(row => row.map(cellValue => cellValue.trim))

  def rangeExists(range: String): Boolean =
    Try(readRange(range)) match
      case Success(_) => true
      case Failure(e) => e match
        case e400: GoogleJsonResponseException =>
          if e.getMessage.contains("Unable to parse range") then false
          else throw e
        case other => throw(other)
  
  def sheetNames: Vector[String] =
    val request = service.spreadsheets.get(spreadsheetId)
    val response = request.execute()
    response.getSheets.asScala.toVector.map(_.getProperties.getTitle)

  def sheetExists(sheetName: String): Boolean = 
    rangeExists(s"$sheetName!A1")

  def createSheet(sheetName: String): Unit =
    val request = Request().setAddSheet(AddSheetRequest().setProperties(SheetProperties().setTitle(sheetName)))
    batchRequest(List(request)).execute()

  def writeRange(range: String, content: SeqTable): Unit =
    val valueRange = ValueRange().setValues(content.asJavaNestedList)
    val request = service.spreadsheets.values.update(spreadsheetId, range, valueRange).setValueInputOption("RAW")
    request.execute()

object SpreadsheetHandle:

  def apply(id: String): Try[SpreadsheetHandle] =
    Try {
      new SpreadsheetHandle(
        "Wuebutab", 
        "auth/credentials.json", 
        "auth/tokens", 
        id)
    }
