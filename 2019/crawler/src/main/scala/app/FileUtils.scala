package app

import java.io.{File, PrintWriter}

import cats.effect.{IO, Resource}
import fs2.INothing
import org.http4s.Uri

import scala.io.{BufferedSource, Source}

object FileUtils {

  def writeSuccessToFile(writer: PrintWriter)(uri: Uri, htmlInfo: Map[String, Option[String]]): Unit = {
    writer.println(s"$uri <+> $htmlInfo")
    writer.flush()
  }

  def writeFailureToFileErrorHandler(writer: PrintWriter)(uri: Uri, e: Throwable): fs2.Stream[fs2.Pure, INothing] = {
    writer.println(s"$uri <-> $e")
    writer.flush()
    fs2.Stream.empty
  }

  def getSource(name: String): Resource[IO, BufferedSource] = {
    Resource.fromAutoCloseable(
      IO.delay(Source.fromResource(name))
    )
  }

  def getFileWriter(name: String): Resource[IO, PrintWriter] = {
    Resource.fromAutoCloseable(
      IO.delay(new PrintWriter(new File(name)))
    )
  }
}
