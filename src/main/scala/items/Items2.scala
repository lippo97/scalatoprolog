package items

object Items2 extends App {

  trait Player {
    def bag: Set[Item]

    def health: Int
  }

  case class Context(
    player: Player
  )

  implicit val myContext: Context = Context(new Player {
    override def bag: Set[Item] = Set()

    override def health: Int = 100

    override def toString: String = "Player object"
  })

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
  trait Effect

  object Effects {
    case class Store(item: Item) extends Effect
    case class RefillHealth(value: Int, player: Player) extends Effect
    case class Hit(what: Entity) extends Effect
    case class LoseHealth(value: Int, player: Player) extends Effect
  }

  trait Entity
  trait Item extends Entity {
    def use(action: TransitiveAction): Option[Effect]
  }

  type Property = PartialFunction[(TransitiveAction, Item), Effect]

  object Item {
    def byProperty(property: Property): Item = new Item { self =>
      override def use(action: TransitiveAction): Option[Effect] = property.lift((action, self))
    }

    def byProperties(properties: Property*): Item = {
      byProperty(properties.reduce(_ orElse _))
    }
  }

  def canBeStored: Property = {
    case (Actions.Take, item) => Effects.Store(item)
  }

//  type Property = PartialFunction[(TransitiveAction, Item), Effect]
  def custom(action: TransitiveAction)(createEffect: (Item, Context) => Effect)(implicit context: Context): Property = {
    case (_action, item) if _action == action => createEffect(item, context)
  }
//    new PartialFunction[(TransitiveAction, Item), Effect] {
//      override def apply(v1: (TransitiveAction, Item)): Effect = v1 match {
//        case (_action, item) if _action == action => createEffect(item, context)
//      }
//
//      override def isDefinedAt(x: (TransitiveAction, Item)): Boolean = x._1 == action
//    }


//  def food(createEvent: Context => Event)(implicit context: Context): Property = {
//    case (Actions.Eat, _) => createEvent(context)
//  }
  def food(createEvent: (Item, Context) => Effect): Property =
    custom(Actions.Eat)(createEvent)

  val apple = Item.byProperties(
    canBeStored,
    food {
      case (_, Context(player)) => Effects.RefillHealth(10, player)
    },
//    custom(Actions.Eat) { case (_, Context(player)) =>
//      Effects.LoseHealth(10, player)
//    }
  )

  val badApple = Item.byProperties(
    canBeStored,
    food { case (_, Context(player)) => Effects.LoseHealth(30, player) }
  )

  case class ResolverResult(
    action: TransitiveAction,
    player: Player,
    item: Item
  )

  def interpreter(resolverResult: ResolverResult): Either[String, Effect] = {
    val ResolverResult(action, _, item) = resolverResult
    (item use action).toRight("You can't do such action.")
  }

  // Simuliamo la fase in cui abbiamo già effettuato il parsing e la risoluzione dei link.
  // ResolverResult(Actions.Take, myContext.player, apple)

  println(interpreter(ResolverResult(Actions.Take, myContext.player, apple)))
  println(interpreter(ResolverResult(Actions.Eat, myContext.player, apple)))
  println(interpreter(ResolverResult(Actions.Hit, myContext.player, apple)))

}
