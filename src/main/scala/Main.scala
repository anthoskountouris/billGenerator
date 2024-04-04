object Main extends MenuItem {

  // creation of the item objects which they includes enumeration
  val cocaCola = ItemCaseClass("Cola - Cold", 0.50, TypeOfItem.drink, Temperature.cold)
  val coffee = ItemCaseClass("Coffee - Hot", 1.00, TypeOfItem.drink, Temperature.hot)
  val cheeseSandwich = ItemCaseClass("Cheese Sandwich - Cold", 2.00, TypeOfItem.food, Temperature.cold)
  val steakSandwich = ItemCaseClass("Steak Sandwich - Hot",4.50, TypeOfItem.food, Temperature.hot)

  // Function that prints the menu
  def printMenu():Unit = {
    println("        Menu")
    println("---------------------")

    println(s"1 - ${cocaCola.name}: £${cocaCola.price}")
    println(s"2 - ${coffee.name}: £${coffee.price}")
    println(s"3 - ${cheeseSandwich.name}: £${cheeseSandwich.price}")
    println(s"4 - ${steakSandwich.name}: £${steakSandwich.price}")
    println("---------------------")
  }

  // Function which calculates the total cost
  def costCalculation(listOfItems:List[ItemCaseClass]): BigDecimal = {
    var percent:BigDecimal = 1
    var serviceCharge:BigDecimal = 0
    val listOfPrices = listOfItems.map(x => x.price)

    if (listOfItems.exists(_.typeOfItem == TypeOfItem.food) && listOfItems.exists(_.temp == Temperature.hot)) {
//      println("I am HERE 1")
      percent = 0.2
      serviceCharge = listOfPrices.sum * percent
    } else if (listOfItems.exists(_.typeOfItem == TypeOfItem.food)){
//      println("I am HERE 2")
      percent = 0.1
      if (listOfPrices.sum * percent > 20){
        serviceCharge = 20
      } else {
        serviceCharge = listOfPrices.sum * percent
      }
    }else {
//      println("I am HERE 3")
      percent = 1
    }
    println(s"The Service Charge is: ${percent*100}%")
    val result = listOfPrices.sum + serviceCharge
    result

  }

  // Function that turns the input number into an item
  // if the user gives "No" the system doesn't ask the user if they need anything else
  // if the user gives something other than the numbers or "No", None is returned
  // Here Option was used because the input can be None
  // Also Either was used because the return value can be either or boolean
  def numberToItem(num:String):Either[Option[ItemCaseClass], Boolean] = {
    if (num=="No") {
      Right(false)
    }else {
      num match {
        case "1" => Left(Some(cocaCola))
        case "2" => Left(Some(coffee))
        case "3" => Left(Some(cheeseSandwich))
        case "4" => Left(Some(steakSandwich))
        case _ => Left(None)
      }
    }

  }

  // Main function
  def main(args: Array[String]): Unit = {

    println("Welcome to Cafe X")
    printMenu()
    print("What would you like? (Number): ")

    var listOfItems:List[Either[Option[ItemCaseClass], Boolean]] = List() // List which stores both the Either ot Boolean
    var FinalListOfItem: List[ItemCaseClass] = List() // List to store the items/objects (without Some()etc)
    var continueLoop = true

    // while loop which asks the user to prompt a number/choice
    while(continueLoop){
      val num = scala.io.StdIn.readLine() // User input
      val item = numberToItem(num) // This value can be Either[Option[ItemCaseClass], Boolean] or None

      // Deconstructing the Either and Option values to normal ones like Boolean, ItemCaseClass or None
      val newItem = item match {
        case Right(x) => x
        case Left(Some(x)) => x
        case _ => None
      }

      if (newItem == false){ // If the value is false, the user don't need anything else
        continueLoop = false
      } else if (newItem == None){ // If it is None, wrong input was given
        print("Would you like something else? (Number/No): ")
      } else {
        listOfItems = listOfItems :+ item
        println(listOfItems)
        FinalListOfItem
        print("Would you like something else? (Number/No): ")
      }
    }

    // Deconstructing Some() to get the ItemCaseClass objects
    FinalListOfItem = listOfItems.map {
      case Left(Some(x)) => x
    }
    println(FinalListOfItem)

    // Calculating the total cost
    cost = costCalculation(FinalListOfItem)
    println(s"Total Cost: $cost")
  }
}
