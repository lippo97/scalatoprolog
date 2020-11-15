package prolog

object Items {

  /* might include other characters as well */
  trait Entity

  trait Item extends Entity
  case object Apple extends Item
  case object Water extends Item

  case class CustomItem(name: String)

}
