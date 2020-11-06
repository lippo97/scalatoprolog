import alice.tuprolog._

object Scala2P {

  /*def extractTerm(solveInfo:SolveInfo, i:Integer): Term =
    solveInfo.getSolution.asInstanceOf[Struct].getArg(i).getTerm

  def extractTerm(solveInfo:SolveInfo, s:String): Term =
    solveInfo.getTerm(s)*/
  def extractTerm(t:Term, i:Integer): Term =
    t.asInstanceOf[Struct].getArg(i).getTerm

  implicit def stringToTerm(s: String): Term = Term.createTerm(s)
  implicit def seqToTerm[T](s: Seq[T]): Term = s.mkString("[",",","]")
  //implicit def stringToTheory[T](s: String): Theory = new Theory(s)

  /*def mkPrologEngine(theory: Theory): Term => Stream[SolveInfo] = {
    val engine = new Prolog
    engine.setTheory(theory)

    goal => new Iterable[SolveInfo]{

      override def iterator = new Iterator[SolveInfo]{
        var solution: Option[SolveInfo] = Some(engine.solve(goal))

        override def hasNext = solution.isDefined &&
                              (solution.get.isSuccess || solution.get.hasOpenAlternatives)

        override def next() =
          try solution.get
          finally solution = if (solution.get.hasOpenAlternatives) Some(engine.solveNext()) else None
      }
    }.toStream
  }

  def solveWithSuccess(engine: Term => Stream[SolveInfo], goal: Term): Boolean =
    engine(goal).map(_.isSuccess).headOption == Some(true)

  def solveOneAndGetTerm(engine: Term => Stream[SolveInfo], goal: Term, term: String): Term =
    engine(goal).headOption map (extractTerm(_,term)) get*/

  def mkPrologEngine(clauses: String*): Term => Stream[Term] = {
    goal => new Iterable[Term]{
      val engine = new Prolog
      engine.setTheory(new Theory(clauses mkString " "))

      override def iterator = new Iterator[Term]{
        var solution = engine.solve(goal);
        override def hasNext = solution.isSuccess ||
                                solution.hasOpenAlternatives

        override def next() =
          try {
            solution.getSolution
          }finally {
            solution = engine.solveNext
          }
      }
    }.toStream
  }

}


object TryScala2P extends App {

  import Scala2P._

  val engine: Term => Stream[Term] = mkPrologEngine(
    """
      load_library('alice.tuprolog.lib.DCGLibrary').
      betareduce(Arg^Expr, Arg, Expr).

      s(S) --> np(Arg), vp(Arg^S).

      / intransitive verb /
      vp(VP) --> iv(VP).
      / transitive verb /
      vp(VP) --> tv(NP^VP), np(NP).
      / ditransitive forms /
      vp(VP) --> vp(2/Pform, VP).
      vp(VP) --> vp(3/Pform, VP).
      vp(VP) --> vp(4/Pform, VP).



      pp(Form, Sem) --> p(Form,X^Sem), np(Sem).

      p(to, ) --> [to].
      p(from, ) --> [from].
      p(in, _) --> [in].

      /
       * Small hint: since vp only cares about Sem,
       * a beta-reduction is automatically performed,
       * without doing it explicitly.
       */
      vp(2/Pform, Sem) -->
          v(2/Pform,Y^Sem),
          pp(Pform,Y).

      vp(3/Pform, Sem) -->
          v(3/Pform,Z^Y^Sem),
          np(Y),
          pp(Pform,Z).

      vp(4/Pform, Sem) -->
          v(4/Pform,Z^Y^X^Sem),
          np(X),
          np(Y),
          pp(Pform,Z).

      tv(X^Y^use(Y,X)) --> [uses].

      v(2/to, Y^X^go(X, Y)) --> [goes].
      v(3/to, Z^Y^X^give(X,Y,Z) ) --> [gives].
      v(3/from, Z^Y^X^buy(X,Y,Z) ) --> [buys].

      np(giuseppe) --> [giuseppe].
      np(ugo) --> [ugo].
      np(gift) --> [a, gift].

      ciao(goro).
      """)

  engine("ciao(X).") foreach (println(_))

}
