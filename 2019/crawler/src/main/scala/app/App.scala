package app

import java.io.PrintWriter
import cats.effect.IO.{Map => _, _}
import cats.effect._
import cats.effect.{IO, Resource}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.{Request, Response, Uri}
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Success, Try}
import scala.xml._
import FileUtils._
import com.typesafe.scalalogging.StrictLogging
import org.http4s.client.middleware.{FollowRedirect, Logger}

object App extends StrictLogging {
  val `5Kb` = 5 * 1024

  def app(ec: ExecutionContext)(implicit ce: ConcurrentEffect[IO]): Resource[IO, IO[Unit]] = {
    for {
      httpClient <- mkHttpClient(ec)
      redirectClient = FollowRedirect(maxRedirects = 2)(httpClient)
      urlsSource    <- getSource("bkshort.txt")
      successWriter <- getFileWriter("success.txt")
      failureWriter <- getFileWriter("failure.txt")
    } yield fs2.Stream
      .fromIterator(urlsSource.getLines)
      .map(x => Uri.fromString("https://" + x))
      .collect { case Right(uri) => uri }
      .parEvalMap(16)(getResponseBody(redirectClient, _, failureWriter))
      .evalMap[IO, Either[Throwable, Unit]] { case (uri, rawBody) =>
        IO.delay(writeSuccessToFile(successWriter)(uri, parseHtml(uri, rawBody))).attempt
      }
      .compile
      .drain
  }

  private def mkHttpClient(ec: ExecutionContext)(implicit ce: ConcurrentEffect[IO]): Resource[IO, Client[IO]] = {
    BlazeClientBuilder[IO](ec)
      .withMaxTotalConnections(24)
      .withConnectTimeout(1.second)
      .withRequestTimeout(5.seconds)
      .resource
  }

  private def getResponseBody(httpClient: Client[IO], uri: Uri, writer: PrintWriter): IO[(Uri, Vector[Byte])] = {
    def checkCode(resp: Response[IO]) = {
      IO.raiseError(new RuntimeException(s"expected code 200 but received ${resp.status.code}"))
    }

    httpClient
      .stream(Request[IO](uri = uri))
      .evalMap(resp => if (resp.status.code == 200) IO.pure(resp) else checkCode(resp))
      .handleErrorWith(writeFailureToFileErrorHandler(writer)(uri, _))
      .flatMap(_.body.take(`5Kb`))
      .compile
      .toVector
      .map((uri, _))
  }

  private def getMetaEntry(nodes: NodeSeq, metaName: String): (String, Option[String]) = {
    val maybeValue =
      nodes
        .find(x => Try(x \@ "name").map(_.toLowerCase) == Success(metaName.toLowerCase))
        .map(x => Try(x \@ "content"))
        .sequence
        .getOrElse(None)

    metaName -> maybeValue
  }

  private def parseHtml(uri: Uri, rawBody: Vector[Byte]): Map[String, Option[String]] = {
    val strBody = new String(rawBody.toArray)
    val xmlBody = Try(XML.loadString(strBody))
    val headTry =
      xmlBody
        .map(_ \ "head")
        .orElse(xmlBody.map(_ \ "html" \ "head"))
        .map(_.headOption.logNotPresent(s"head tag is not present in $uri"))

    headTry match {
      case Success(Some(head)) =>
        val maybeTitle = Try((head \ "title").head).toOption.logNotPresent(s"title tag is not present in $uri")
        val maybeMetas = Try(head \ "meta").toOption.logNotPresent(s"meta tag is not present in $uri")

        val titleMap = Map("title" -> maybeTitle.map(_.text))
        val metaMap =
          maybeMetas match {
            case None =>
              Map.empty[String, Option[String]]
            case Some(metas) =>
              Map(getMetaEntry(metas, "keywords"), getMetaEntry(metas, "description"))
          }
        titleMap ++ metaMap
      case _ =>
        Map.empty[String, Option[String]]
    }
  }

  implicit class RichParsingOption[T](val maybeNode: Option[T]) extends AnyVal {
    def logNotPresent(failMessage: String): Option[T] = {
      maybeNode match {
        case Some(value) =>
          Some(value)
        case None =>
          logger.warn(failMessage)
          None
      }
    }
  }
}
