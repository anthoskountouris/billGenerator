object Main extends MenuItem {

  // creation of the item objects which they includes enumeration
  val cocaCola = ItemCaseClass("Cola - Cold", 0.50, TypeOfItem.drink, Temperature.cold, Category.nonPremium)
  val coffee = ItemCaseClass("Coffee - Hot", 1.00, TypeOfItem.drink, Temperature.hot, Category.nonPremium)
  val cheeseSandwich = ItemCaseClass("Cheese Sandwich - Cold", 2.00, TypeOfItem.food, Temperature.cold, Category.nonPremium)
  val steakSandwich = ItemCaseClass("Steak Sandwich - Hot",4.50, TypeOfItem.food, Temperature.hot, Category.nonPremium)
  val lobster = ItemCaseClass("Lobster", 25, TypeOfItem.food, Temperature.hot, Category.premium)

  // Function that prints the menu
  def printMenu():Unit = {
    println("        Menu")
    println("---------------------")

    println(s"1 - ${cocaCola.name}: £${cocaCola.price} (${if (cocaCola.cat == Category.nonPremium) "Non Premium" else "Premium"})")
    println(s"2 - ${coffee.name}: £${coffee.price} (${if (coffee.cat == Category.nonPremium) "Non Premium" else "Premium"})")
    println(s"3 - ${cheeseSandwich.name}: £${cheeseSandwich.price} (${if (cheeseSandwich.cat == Category.nonPremium) "Non Premium" else "Premium"})")
    println(s"4 - ${steakSandwich.name}: £${steakSandwich.price} (${if (steakSandwich.cat == Category.nonPremium) "Non Premium" else "Premium"})")
    println(s"5 - ${lobster.name}: £${lobster.price} (${if (lobster.cat == Category.nonPremium) "Non Premium" else "Premium"})")
    println("---------------------")
  }

  // Function which calculates the total cost
  def costCalculation(listOfItems:List[ItemCaseClass], loyaltyPerc:BigDecimal, loyaltyAmount:BigDecimal): BigDecimal = {
    var percent:BigDecimal = 1 // percentage of service charge
    var serviceCharge:BigDecimal = 0 // actual amount of service charge
    val listOfPrices = listOfItems.map(x => x.price) // List of the prices of the items in the basket

    // Conditions to add service charge in the total cost
    if (listOfItems.exists(_.cat == Category.premium)){
      percent = 0.25
      if (listOfPrices.sum * percent > 40){
        serviceCharge = 40
      } else {
        serviceCharge = (listOfPrices.sum * percent).setScale(2, BigDecimal.RoundingMode.HALF_UP)
      }
    }
    else if (listOfItems.exists(_.typeOfItem == TypeOfItem.food) && listOfItems.exists(_.temp == Temperature.hot)) {
      percent = 0.2
      serviceCharge = (listOfPrices.sum * percent).setScale(2, BigDecimal.RoundingMode.HALF_UP)
    } else if (listOfItems.exists(_.typeOfItem == TypeOfItem.food)){
      percent = 0.1
      if (listOfPrices.sum * percent > 20){
        serviceCharge = 20
      } else {
        serviceCharge = (listOfPrices.sum * percent).setScale(2, BigDecimal.RoundingMode.HALF_UP)
      }
    }else {
      percent = 1
    }
    println(s"Total before discount and service charge: £${listOfPrices.sum}")
    println(s"Your loyalty discount is: £${loyaltyAmount} ${if (loyaltyPerc == 1){""} else {"(" + loyaltyPerc +"%)"} }")
    println(s"The Service Charge is: £${serviceCharge} ${ if(percent==1){""} else {"(" + (percent*100).toInt + "%)"}}")
    val result = listOfPrices.sum - loyaltyAmount + serviceCharge
    result

  }

  // Function that turns the input number into an item
  // if the user gives "No" the system doesn't ask the user if they need anything else
  // if the user gives something other than the numbers or "No", None is returned
  // Here Option was used because the input can be None
  // Also Either was used because the return value can be either or boolean
  def numberToItem(num:String):Either[Option[ItemCaseClass], Boolean] = {
    if (num == "no") {
      Right(false)
    } else {
      num match {
        case "1" => Left(Some(cocaCola))
        case "2" => Left(Some(coffee))
        case "3" => Left(Some(cheeseSandwich))
        case "4" => Left(Some(steakSandwich))
        case "5" => Left(Some(lobster))
        case _ => Left(None)
      }
    }
  }

    def printBasket(listOfItems:List[ItemCaseClass]): Unit ={

      val counts = listOfItems.groupBy(identity).mapValues(_.size).toMap
//      println(counts)
      println("        Bill         ")
      println("---------------------")
      counts.foreach {
        case (key, value) => println(s"${key.name}: £${key.price} x $value")
      }
      println("---------------------")
    }

  // Checking for Loyalty Card
  def askingForLoyalty(listOfItems:List[ItemCaseClass]):(BigDecimal,BigDecimal) ={
    val sumOfBasket = listOfItems.map(x => x.price).sum

    print("Do you have a Loyalty card mate? (yes or no): ")
    val hasLoyalty = scala.io.StdIn.readLine() // User input

    var loyaltyAmount:BigDecimal = 0

    if (hasLoyalty=="no"){
      (1,loyaltyAmount)
    }else if(hasLoyalty=="yes"){
      print("How many stars do you have: ")
      val startsNo = scala.io.StdIn.readLine().toInt // User input
      val loyaltyPercentage:BigDecimal = startsNo match {
        case x if 3 <= x && x <= 8 => 0.025 * x
        case x if x>8 => 0.025 * 8
        case _ => 0
      }
      val loyaltyPercentageModified: BigDecimal = (loyaltyPercentage*100).setScale(2, BigDecimal.RoundingMode.HALF_UP)
//      println("loyaltyPercentage: " + loyaltyPercentageModified)
      loyaltyAmount = loyaltyAmount + (loyaltyPercentage * sumOfBasket)
//      println("loyaltyAmount: " + loyaltyAmount)

      (loyaltyPercentageModified.toInt,loyaltyAmount)

    } else{
      askingForLoyalty(listOfItems)
    }
  }

//  def askingForStars(yesOrNo: String):BigDecimal ={
//
//  }

  // Main function
  def main(args: Array[String]): Unit = {

    println("Welcome to Cafe X")
    printMenu()
    print("What would you like? (number): ")

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
        print("Would you like something else? (number or no): ")
      } else {
        listOfItems = listOfItems :+ item
//        println(listOfItems)
//        FinalListOfItem
        print("Would you like something else? (number or no): ")
      }
    }

    // Deconstructing Some() to get the ItemCaseClass objects
    FinalListOfItem = listOfItems.map {
      case Left(Some(x)) => x
    }

    val (loyaltyPerc:BigDecimal, loyaltyAmount:BigDecimal) = askingForLoyalty(FinalListOfItem)
//     Printing the basket
    printBasket(FinalListOfItem)
    // Calculating the total cost
    cost = costCalculation(FinalListOfItem,loyaltyPerc,loyaltyAmount).setScale(2, BigDecimal.RoundingMode.HALF_UP)
    println(s"Total Cost: £$cost")
  }
}