package items

import items.Items2.Item.{Contextualizable, Item}

object Items2 extends App {

  trait Player {
    def bag: Set[Item]

    def health: Int
  }

  case class SimplePlayer(
    bag: Set[Item] = Set(),
    health: Int = 50
  ) extends Player {
    override def toString: String = {
      s"Player(health = ${health}, ${bag.toString})"
    }
  }

  implicit val myContextProvider: ContextProvider = state.context _

  trait TransitiveAction

  object Actions {
    case object Take extends TransitiveAction
    case object Eat extends TransitiveAction
    case object Hit extends TransitiveAction
  }

  trait Effect {
    def update(gameState: GameState): GameState
  }

  object Effects {

//    def store(item: Item, player: SimplePlayer): Effect = (gameState: GameState) => ???

    case class Store(item: Item, player: SimplePlayer) extends Effect {
      override def update(gameState: GameState): GameState = gameState.copy(
        player = gameState.player.copy(
          bag = gameState.player.bag + item
        ),
        room = gameState.room - item
      )
    }
    def alterHealth(gameState: GameState, f: Int => Int): GameState = gameState.copy(
      player = gameState.player.copy(
        health = f(gameState.player.health)
      )
    )

//    case class ConsumeItem(item: Item) extends Effect {
//      override def update(gameState: GameState): GameState = gameState.
//    }

    case class GainHealth(value: Int, player: Player) extends Effect {
      override def update(gameState: GameState): GameState = alterHealth(gameState, _ + value)
    }
    case class LoseHealth(value: Int, player: Player) extends Effect {
      override def update(gameState: GameState): GameState = alterHealth(gameState, _ - value)
    }
    case object Splash extends Effect {
      override def update(gameState: GameState): GameState = gameState
      // https://media.tenor.com/images/5160e395584d631e4ca24795b70ecf49/tenor.gif
      // Nothing happened!
    }
  }

  trait Entity

  type Property = PartialFunction[(TransitiveAction, Item, Context), Effect]

  object Item {

    trait Item extends Entity {
      def name: String
    }

    trait Behavior extends Item {
      def property: Property
    }

    trait Contextualizable extends Item {
      def contextualize(context: Context): ContextualizedItem
    }

    case class SimpleItem(
      name: String,
      property: Property = PartialFunction.empty
    ) extends Item with Contextualizable with Behavior {
      override def contextualize(context: Context): ContextualizedItem = new SimpleContextualizedItem(this, context)
    }

    trait ContextualizedItem {
      def use(action: TransitiveAction): Option[Effect]
    }

    class SimpleContextualizedItem(
      item: Item with Behavior,
      private val context: Context
    ) extends ContextualizedItem {

      override def use(action: TransitiveAction): Option[Effect] =
        item.property.lift((action, item, context))
    }

    def byProperty(name: String, property: Property): SimpleItem = SimpleItem(name, property)

    def byProperties(name: String, properties: Property*): SimpleItem = {
      SimpleItem(name, properties.reduce(_ orElse _))
    }

    def onlyIfDenture(context: Context): ContextualizedItem = new ContextualizedItem {
      def item: Item = SimpleItem("appleOnlyIfDenture")

      override def use(action: TransitiveAction): Option[Effect] = action match {
        case Actions.Eat => context match {
          case Context.getPlayer(player) if player.bag contains appleDentures => Option(Effects.GainHealth(10, player))
          case _ => Option.empty
        }
        case _ => Option.empty
      }
    }
  }

  def canBeStored: Property = {
    case (Actions.Take, item, context) =>
      Effects.Store(item, context.player)
  }

  def custom(action: TransitiveAction)(createEffect: (Item, Context) => Effect): Property = {
    case (_action, item, context) if _action == action => createEffect(item, context)
  }

  def food(createEvent: (Item, Context) => Effect): Property =
    custom(Actions.Eat)(createEvent)

  val appleDentures = Item.byProperties(
    "apple dentures",
    canBeStored
  )

  val apple = Item.byProperties(
    "apple",
    canBeStored,
    food {
      case (item, Context.getPlayer(player)) =>
        if (player.bag contains appleDentures)
//          Effects.ConsumeItem(item)
          Effects.GainHealth(10, player)
        else
          Effects.Splash
    },
  )

  val badApple = Item.byProperties(
    "bad apple",
    canBeStored,
    food { case (_, Context.getPlayer(p)) => Effects.LoseHealth(30, p) }
  )

  case class ResolverResult(
    action: TransitiveAction,
    player: Player,
    item: Item with Contextualizable
  )

  def interpreter(resolverResult: ResolverResult)(implicit contextProvider: ContextProvider): Either[String, Effect] = {
    val ResolverResult(action, _, item) = resolverResult
    (item contextualize contextProvider() use action).toRight("You can't do such action.")
  }

  type ContextProvider = () => Context

  trait Context {
    def player: SimplePlayer

    def room: Set[Entity]
  }

  object Context {
    object getPlayer {
      def unapply(arg: Context): Option[Player] = Some(arg.player)
    }
  }

  case class GameState(
    player: SimplePlayer,
    room: Set[Entity] = Set()
  ) extends Context {
    def context: Context = this
  }

  val player = SimplePlayer()

  var state = GameState(player, room = Set(apple, appleDentures, badApple))

  println(state)
  Seq(
    () => interpreter(ResolverResult(Actions.Take, state.player, appleDentures)),
    () => interpreter(ResolverResult(Actions.Eat, state.player, apple)),
    () => interpreter(ResolverResult(Actions.Eat, state.player, badApple))
  ).toStream map { _() }  foreach {
    case Left(err) => println(err)
    case Right(eff) =>
      state = eff update state
      println("after eff: " + eff)
      println(state)
  }
}
