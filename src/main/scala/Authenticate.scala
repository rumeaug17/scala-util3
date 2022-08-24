package org.rg.su3

import scala.sys.SystemProperties
import java.net.{ Authenticator, PasswordAuthentication }

inline def authenticate(domain: String,
                        login: String,
                        passwd: String,
                        host: String = "localhost",
                        port: Int = 80,
                        nonProxyHosts: String = "localhost|127.0.0.1|.loc|"): Unit =
  val props = new SystemProperties()
  props.update("http.proxyHost", host)
  props.update("http.proxyPort", port.toString)
  props.update("https.proxyHost", host)
  props.update("https.proxyPort", port.toString)

  props.update("proxy.authentication.username", login)
  props.update("proxy.authentication.password", passwd)

  props.update("http.auth.preference", "ntlm")
  props.update("http.auth.ntlm.domain", domain)
  props.update("https.auth.preference", "ntlm")
  props.update("https.auth.ntlm.domain", domain)

  props.update("http.nonProxyHosts", nonProxyHosts)
  props.update("https.nonProxyHosts", nonProxyHosts)

  props.update("javax.net.ssl.trustStoreType", "WINDOWS-ROOT")

  Authenticator.setDefault(new Authenticator() {
    override def getPasswordAuthentication: PasswordAuthentication =
      new PasswordAuthentication(login, passwd.toCharArray())    
  })
