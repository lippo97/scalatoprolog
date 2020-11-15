package prolog

import prolog.Items.Item

object VerbsUtils {

  trait Action

  case object Eat extends Action
  case object Drink extends Action
  case object PickUp extends Action
  case object Give extends Action

  trait SemanticG[A] {
    def n: Int

    def action: A
  }

  trait Semantic extends SemanticG[Action]

  trait TransitiveSemantic extends Semantic {
    override def n: Int = 2
  }

  trait DitransitiveSemantic extends Semantic {
    override def n: Int = 3
  }

  case class CustomSemantic(n: Int, action: Action) extends Semantic

  case object EatSem extends Semantic with TransitiveSemantic {
    override def action: Action = Eat
  }

  case object DrinkSem extends Semantic with TransitiveSemantic {
    override def action: Action = Drink
  }

  case object TakeSem extends Semantic with TransitiveSemantic {
    override def action: Action = PickUp
  }

  case object GiveSem extends Semantic with DitransitiveSemantic {
    override def action: Action = Give
  }

  case class Verb(name: String, semantic: Semantic) {

    def functor: String = name.replace(" ", "_")

    def tokens: String = "[" + name.split(" ").reduce(_+","+_) + "]"

    def fact: String = {
      s"tv(Y^X^${functor}(X, Y)) --> ${tokens}."
    }
  }

  object Verb {
    object Eat extends Verb("eat", EatSem)
    object Drink extends Verb("drink", DrinkSem)
    object PickUp extends Verb("pick up", TakeSem)
    object Take extends Verb("take", TakeSem)

  }

}
