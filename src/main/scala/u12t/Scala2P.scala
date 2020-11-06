package u12t

import alice.tuprolog._
import alice.tuprolog.lib.DCGLibrary
import cats.effect.{IO, Resource}

import scala.io.{BufferedSource, Source}

object Scala2P extends App {
  val myGoal = "phrase(s(X), [giuseppe, buys, a, gift, from, Y])."

  def makeFileResource(path: String): Resource[IO, BufferedSource] = {
    Resource.make {
      IO(Source.fromResource(path))
    } { in =>
      IO(in.close())
    }
  }

  def exploreSolutions(engine: Prolog, goal: String): LazyList[SolveInfo] = {
    var solution = engine.solve(goal)

    new Iterator[SolveInfo] {
      override def hasNext: Boolean = solution.isSuccess && solution.hasOpenAlternatives

      override def next(): SolveInfo =
        try { solution } finally {solution = engine.solveNext }
    }.to(LazyList)
  }

  val res = for {
    t <- makeFileResource("grammar.pl")
        .map(_.mkString)
        .use { src =>
          IO {
            new Theory(src)
          }
        }.attempt
    e <- IO {
      t.map { src =>
        val engine = new Prolog
        engine.loadLibrary(new DCGLibrary)
        engine.setTheory(src)
        engine
      }
    }
    s <- IO {
      e.map {
        exploreSolutions(_, myGoal).map(_.getTerm("X"))
      }
    }

  } yield s

  res.unsafeRunSync() match {
    case Left(value) => println(value)
    case Right(solutions) => solutions.foreach(println(_))
  }
}
