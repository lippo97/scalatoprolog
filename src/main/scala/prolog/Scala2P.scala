package prolog

import alice.tuprolog
import alice.tuprolog.lib.DCGLibrary
import alice.tuprolog.{AbstractSocket, Prolog, SolveInfo, Struct, Term, Theory, Var}
import cats.effect.{IO, Resource}
import prolog.Items.{Apple, Item, Water}
import prolog.VerbsUtils.{DrinkSem, EatSem, Semantic, TakeSem, Verb}

import scala.io.{BufferedSource, Source}

object Scala2P extends App {

  implicit class EnhancedStruct(struct: Struct) {
    def getTerms(n: Int): Seq[Term] = {
      var terms = scala.collection.mutable.ListBuffer[Term]()
      for (i <- 0 until n) {
        terms += struct.getTerm(i)
      }
      terms.toSeq
    }
  }

  implicit def toVerb(pair: (String, Semantic)): Verb =
    Verb(pair._1, pair._2)

  val verbs: List[Verb] = List(
//    Verb("eat", EatSem),
    "eat" -> EatSem,
    "drink" -> DrinkSem,
    "pick up" -> TakeSem,
    "take" -> TakeSem
  )

  val facts = verbs.map(_.fact).reduce(_+"\n"+_)

  val stringToItem: String => Item = Map(
    "apple" -> Apple,
    "water" -> Water,
  )

  val verbsToSemantic: Map[String, Semantic] = (verbs map (v => v.functor -> v.semantic)).toMap

  val command = "pick up an apple" // pick_up(you, apple).
  val myGoal = s"phrase(i(X), ${
    "[" + command.split(" ").reduce(_ + ", " + _) + "]"
  })."

  def makeFileResource(path: String): Resource[IO, BufferedSource] = {
    Resource.make {
      IO(Source.fromResource(path))
    } { in =>
      IO(in.close())
    }
  }

  def exploreSolutions(engine: Prolog, goal: String): Stream[SolveInfo] = {
    var solution = engine.solve(goal)

    new Iterator[SolveInfo] {
      override def hasNext: Boolean = solution.isSuccess && solution.hasOpenAlternatives

      override def next(): SolveInfo =
        try { solution } finally {solution = engine.solveNext }
    }.toStream
  }

  val res = for {
    t <- makeFileResource("grammar.pl")
        .map(_.mkString)
        .map(_ + "\n" + facts)
        .map(theory => {
          println(theory)
          theory
        })
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
        exploreSolutions(_, myGoal) take 1 map { sol =>
          val struct = sol.getTerm("X").asInstanceOf[Struct]
          println(struct)
          val sem = verbsToSemantic(struct.getName)
          case object You
          val terms = struct.getTerms(sem.n) map(_.toString) map {
            case "you" => You
            case x => stringToItem(x)
          }
          (sem, terms)
        }
      }
    }
  } yield s

  res.unsafeRunSync() match {
    case Left(value) => println(value)
    case Right(solutions) => solutions.foreach(println(_))
  }
}
