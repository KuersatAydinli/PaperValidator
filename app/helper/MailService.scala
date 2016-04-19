package helper

import java.net.InetAddress
import java.util.Properties
import javax.mail._
import javax.mail.internet._

import play.api.Logger

object MailService
{
  val FROM = "***REMOVED***"
  val PW = "***REMOVED***!"
  val HOST = "smtp.gmail.com"
  val PORT = "587"

  def sendMail(to: String,
           subject: String,
           content: String) : Unit = {

    val inet = InetAddress.getByName("www.google.com")
    if(!inet.isReachable(5000)) {
      Logger.debug("Mail Server not reachable!")
      return
    }


    val props = new Properties()
    props.put("mail.smtp.host", HOST)
    props.put("mail.smtp.port", PORT)
    props.put("mail.smtp.auth", "true")
    props.put("mail.smtp.starttls.enable", "true")

    def getPasswordAuthentication : Authenticator=
    {
      new Authenticator(){
        override def getPasswordAuthentication():PasswordAuthentication = {
          new PasswordAuthentication(FROM, PW);
        }}
    }

    val session = Session.getInstance(props, getPasswordAuthentication)


    // Create a default MimeMessage object.
    val message = new MimeMessage(session)

    // Set From: header field of the header.
    message.setFrom(new InternetAddress(FROM))

    // Set To: header field of the header.
    message.addRecipient(Message.RecipientType.TO,
      new InternetAddress(to))

    // Set Subject: header field
    message.setSubject(subject)

    // Now set the actual message
    message.setText(content)

    Transport.send(message);

  }

}