package items

object ItemsOld extends App {

  trait Player {
    def bag: Set[Item]

    def health: Int
  }

  case class Context(
    player: Player
  )

  implicit val myContext: Context = Context {
    new Player {
      override def bag: Set[Item] = Set()

      override def health: Int = 100

      override def toString: String = "Player object"
    }
  }

  // Azione rappresenta univocamente la possibilità di effettuare una mossa all'interno del gioco.
  // Ad un unica azione possono corrispondere più verbi.
  // es: String("Pick up") .. Take
  //     String("take") ..... Take
  //     String("get") ...... Take
  trait TransitiveAction

  object Actions {
    case object Take extends TransitiveAction
    case object Eat extends TransitiveAction
    case object Hit extends TransitiveAction
  }

  // Event rappresenta il risultato POSITIVO di un'azione del giocatore.
  // Idealmente dovrebbe essere un oggetto che codifica le modifiche da fare
  // al player, all'environment, allo spazio, a tutto.
  trait Event
  case class Store(item: Item) extends Event
  case class RefillHealth(value: Int, player: Player) extends Event

  trait Item {
    def use(action: TransitiveAction): Option[Event]
  }

  object Item {
    def byPartialFunction(property: PartialFunction[(TransitiveAction, Item), Event]): Item = new Item { self =>
      override def use(action: TransitiveAction): Option[Event] = property.lift((action, self))
    }

    def byPartialFunctions(properties: PartialFunction[(TransitiveAction, Item), Event]*): Item = {
      byPartialFunction(properties.reduce(_ orElse _))
    }
  }

  def canBeStored: PartialFunction[(TransitiveAction, Item), Event] = {
    case (Actions.Take, item) => Store(item)
  }

  def food(createEvent: Context => Event)(implicit context: Context): PartialFunction[(TransitiveAction, Item), Event] = {
     case (Actions.Eat, _) => createEvent(context)
  }

  val apple = Item.byPartialFunctions(
    canBeStored,
    food { case Context(player) => RefillHealth(10, player) }
  )



  case class ResolverResult(
    action: TransitiveAction,
    player: Player,
    item: Item
  )

  def interpreter(resolverResult: ResolverResult)(implicit context: Context): Either[String, Event] = {
    val ResolverResult(action, _, item) = resolverResult
    (item use action).toRight("You can't do such action.")
  }

  println(interpreter(ResolverResult(Actions.Take, myContext.player, apple)))
  println(interpreter(ResolverResult(Actions.Eat, myContext.player, apple)))
  println(interpreter(ResolverResult(Actions.Hit, myContext.player, apple)))

}
