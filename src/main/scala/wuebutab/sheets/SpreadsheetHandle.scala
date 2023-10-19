package wuebutab

import com.google.api.services.sheets.v4.SheetsScopes
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets
import com.google.api.client.json.gson.GsonFactory
import java.io.FileReader
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import scala.jdk.CollectionConverters._
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.services.sheets.v4.Sheets

class SpreadsheetHandle(
  val applicationName: String,
  val credentialsPath: String,
  val tokensPath: String,
  val spreadsheetId: String):

  private val transp = GoogleNetHttpTransport.newTrustedTransport

  private val flow = 
    GoogleAuthorizationCodeFlow.Builder(
      transp, 
      GsonFactory.getDefaultInstance, 
      GoogleClientSecrets.load(
        GsonFactory.getDefaultInstance,
        FileReader(credentialsPath)),
      List(SheetsScopes.SPREADSHEETS).asJava)
    .setDataStoreFactory(FileDataStoreFactory(java.io.File(tokensPath)))
    .setAccessType("offline")
    .build

  private val receiver =
    LocalServerReceiver.Builder().setPort(8888).build

  private val cred = 
    AuthorizationCodeInstalledApp(flow, receiver).authorize("user")

  private val service = 
    new Sheets.Builder(
      transp, 
      GsonFactory.getDefaultInstance(),
      cred)
      .setApplicationName(applicationName)
      .build
        
  def getTitle =
    service
    .spreadsheets
    .get(spreadsheetId)
    .execute
    .getProperties()
    .getTitle()

  def readRange(range: String): Vector[Vector[String]] =
    service
      .spreadsheets
      .values
      .get(spreadsheetId, range)
      .execute
      .getValues
      .asScala
      .toVector
      .map(_
        .asScala
        .toVector
        .map(_
          .toString))
