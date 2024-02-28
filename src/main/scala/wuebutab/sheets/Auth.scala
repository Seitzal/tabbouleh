package wuebutab

import java.io.FileReader
import scala.jdk.CollectionConverters._

import com.google.api.client.googleapis.auth.oauth2._
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp

import com.google.api.services.sheets.v4.SheetsScopes

class Auth(
  val credentialsPath: String,
  val tokensPath: String,
  val transport: NetHttpTransport):

  private val flow = 
    GoogleAuthorizationCodeFlow.Builder(
      transport, 
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

  val credential = 
    AuthorizationCodeInstalledApp(flow, receiver).authorize("user")
