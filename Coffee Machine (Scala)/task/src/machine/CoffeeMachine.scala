package machine

import scala.io.StdIn._

object Main extends App {
  private val coffeeMachine = new CoffeeMachine

  while (true) {
    coffeeMachine.executeCommand(readLine())
  }
}

class CoffeeMachine{
  private var water = 400
  private var milk = 540
  private var coffeeBeans = 120
  private var cups = 9
  private var money = 550
  private var state: State = MainMenu

  initMainMenu()

  def executeCommand(command: String): Unit = state match
    case MainMenu => mainMenuExecutor(command)
    case BuyMenu => buyMenuExecutor(command)
    case _ => fillMenuExecutor(command)

  private def mainMenuExecutor(command: String): Unit = command match
    case "buy" =>
      changeStateWithMessage(
        newState = BuyMenu,
        message = "What do you want to buy? 1 - espresso, 2 - latte, 3 - cappuccino, back - to main menu:"
      )
    case "fill" =>
      changeStateWithMessage(
        newState = FillWater,
        message = "Write how many ml of water you want to add:"
      )
    case "take" =>
      println()
      println(s"I gave you ${"$" + money}")
      money = 0
      initMainMenu()
    case "remaining" =>
      printStatus()
      initMainMenu()
    case "exit" => System.exit(0)

  private def buyMenuExecutor(command: String): Unit = {
    drinkSelector(command) match
      case Some(drink) =>
        val checkResult = checkResources(drink)
        if (checkResult == "OK")
          println("I have enough resources, making you a coffee!")
          prepareDrink(drink)
        else
          println(s"Sorry, not enough $checkResult!")
      case None => print("")
    initMainMenu()
  }

  private def fillMenuExecutor(command: String): Unit = command.toInt match
    case value if state == FillWater =>
      water = water + value
      changeStateWithMessage(
        newState = FillMilk,
        message = "Write how many ml of milk you want to add:"
      )
    case value if state == FillMilk =>
      milk = milk + value
      changeStateWithMessage(
        newState = FillCoffeeBeans,
        message = "Write how many grams of coffee beans you want to add:"
      )
    case value if state == FillCoffeeBeans =>
      coffeeBeans = coffeeBeans + value
      changeStateWithMessage(
        newState = FillCups,
        message = "Write how many disposable cups you want to add:"
      )
    case value if state == FillCups =>
      cups = cups + value
      initMainMenu()

  private def initMainMenu(): Unit =
    changeStateWithMessage(
      newState = MainMenu,
      message = "Write action (buy, fill, take, remaining, exit): "
    )

  private def changeStateWithMessage(newState: State, message: String): Unit = {
    println()
    println(message)
    state = newState
  }

  private def printStatus(): Unit = println(
    s"""
       |The coffee machine has:
       |$water ml of water
       |$milk ml of milk
       |$coffeeBeans g of coffee beans
       |$cups disposable cups
       |${"$" + money} of money""".stripMargin)

  private def drinkSelector(drinkType: String): Option[Drink] = drinkType match
    case "1" => Some(Espresso())
    case "2" => Some(Latte())
    case "3" => Some(Cappuccino())
    case "back" => None

  private def prepareDrink(drink: Drink): Unit = {
    water = water - drink.water
    milk = milk - drink.milk
    coffeeBeans = coffeeBeans - drink.coffeeBeans
    cups = cups - 1
    money = money + drink.cost
  }

  private def checkResources(drink: Drink): String = {
    if (this.water < drink.water) "water"
    else if (this.milk < drink.milk) "milk"
    else if (this.coffeeBeans < drink.coffeeBeans) "coffee beans"
    else if (this.cups == 0) "disposable cups"
    else "OK"
  }
}

sealed trait Drink{
  def water: Int
  def milk: Int
  def coffeeBeans: Int
  def cost: Int
}
class Espresso extends Drink {
  def water: Int = 250
  def milk: Int = 0
  def coffeeBeans: Int = 16
  def cost = 4
}
class Latte extends Drink {
  def water: Int = 350
  def milk: Int = 75
  def coffeeBeans: Int = 20
  def cost = 7
}
class Cappuccino extends Drink {
  def water: Int = 200
  def milk: Int = 100
  def coffeeBeans: Int = 12
  def cost = 6
}

sealed trait State
case object MainMenu extends State
case object BuyMenu extends State
case object FillWater extends State
case object FillMilk extends State
case object FillCoffeeBeans extends State
case object FillCups extends State