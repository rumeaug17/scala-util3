package org.rg.su3

import scala.sys.SystemProperties
import java.net.{ Authenticator, PasswordAuthentication }

inline def authenticate(domain: String, login: String, passwd: String, host: String = "localhost", port: Int = 80): Unit =
  val props = new SystemProperties()
  props.update("http.proxyHost", host)
  props.update("http.proxyPort", port.toString)
  props.update("http.proxyUser", login)
  props.update("http.proxyPassword", passwd)

  props.update("http.auth.preference", "ntlm")
  props.update("http.auth.ntlm.domain", domain)

  props.update("https.proxyHost", host)
  props.update("https.proxyPort", port.toString)
  props.update("https.proxyUser", login)
  props.update("https.proxyPassword", passwd)

  props.update("https.auth.preference", "ntlm")
  props.update("https.auth.ntlm.domain", domain)

  Authenticator.setDefault(new Authenticator() {
    override def getPasswordAuthentication(): PasswordAuthentication =
      new PasswordAuthentication(login, passwd.toCharArray())    
  })
