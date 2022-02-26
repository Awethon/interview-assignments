package app

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.ExecutionContext

object AppStarter extends IOApp {
  val exec = ExecutionContext.fromExecutor(new java.util.concurrent.ForkJoinPool(32))
  override implicit val contextShift = IO.contextShift(exec)

  override def run(args: List[String]): IO[ExitCode] = {
    App.app(exec).use(_.map(_ => ExitCode.Success))
  }

}
