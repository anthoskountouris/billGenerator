import org.scalatest.flatspec.AnyFlatSpec
import Main.{askingForLoyalty, cheeseSandwich, cocaCola, coffee, costCalculation, lobster, numberToItem, serviceCharge, steakSandwich}
import Temperature.{cold, hot}
import TypeOfItem.{drink, food}
import Category.{nonPremium, premium}


class FunctionalitySpec extends AnyFlatSpec {

  "numberToItem" should "// Function that turns the input number into an item\n  // if the user gives \"No\" the system doesn't ask the user if they need anything else\n  // if the user gives something other than the numbers or \"No\", None is returned" in {
    assert(numberToItem("no") === Right(false))
    assert(numberToItem("1") === Left(Some(cocaCola)))
    assert(numberToItem("2") === Left(Some(coffee)))
    assert(numberToItem("3") === Left(Some(cheeseSandwich)))
    assert(numberToItem("4") === Left(Some(steakSandwich)))
    assert(numberToItem("5") === Left(Some(lobster)))
    // anything but the required responses ->
    assert(numberToItem("evrerv") === Left(None))
    assert(numberToItem("6") === Left(None))
    assert(numberToItem("") === Left(None))
  }

  "askingForLoyalty" should "check the if the user has a loyalty card and return the discount percentage and amount" in {
    val listOfItems1: List[ItemCaseClass] = List(cocaCola, coffee, cheeseSandwich, steakSandwich)

    // User has loyalty card, with no premium products
    val yes: String = "yes"
    // 1x0 = 0
    val starsNo1: Int = 1
    assert(askingForLoyalty(listOfItems1, yes, starsNo1) === (0, 0.0))
    // 3*0.025*100 = 7.5
    // 8*0.075 = 2.48
    val starsNo3: Int = 3
    assert(askingForLoyalty(listOfItems1, yes, starsNo3) === (7.5, 0.60))
    // 10 * 0.025*100 = 25% => 20%
    // 8*0.20 = 6.60
    val starsNo10: Int = 10
    assert(askingForLoyalty(listOfItems1, yes, starsNo10) === (20, 1.60))

    // User has loyalty card, with premium products(dicount is not applied to premium items)
    val listOfItems2: List[ItemCaseClass] = List(cocaCola, coffee, cheeseSandwich, steakSandwich, lobster, lobster)

    // 3*0.025*100 = 7.5
    // 58*0.075 = 2.48
    assert(askingForLoyalty(listOfItems2, yes, starsNo3) === (7.5, 0.60))
    // 10 * 0.025*100 = 25% => 20%
    // 8*0.20 = 6.60
    assert(askingForLoyalty(listOfItems2, yes, starsNo10) === (20, 1.60))


    // User does not have loyalty card
    val no: String = "no"
    assert(askingForLoyalty(listOfItems1, no, 0) === (1, 0.0))
  }

  "serviceCharge" should "calculate service charge" in {
    // Only drinks should return 0 service charge
    val listOfItems1: List[ItemCaseClass] = List(coffee, coffee, coffee, coffee, coffee, coffee)
    assert(serviceCharge(listOfItems1) === 0)

    // includes any cold food, should return 10% of the total bill
    val listOfItems2: List[ItemCaseClass] = List(ItemCaseClass("Coffee - Hot", 1.0, drink, hot, nonPremium), ItemCaseClass("Coffee - Hot", 1.0, drink, hot, nonPremium), ItemCaseClass("Coffee - Hot", 1.0, drink, hot, nonPremium), ItemCaseClass("Coffee - Hot", 1.0, drink, hot, nonPremium), ItemCaseClass("Coffee - Hot", 1.0, drink, hot, nonPremium), ItemCaseClass("Coffee - Hot", 1.0, drink, hot, nonPremium), ItemCaseClass("Cheese Sandwich - Cold", 2.0, food, cold, nonPremium), ItemCaseClass("Cheese Sandwich - Cold", 2.0, food, cold, nonPremium), ItemCaseClass("Cheese Sandwich - Cold", 2.0, food, cold, nonPremium))
    // 6 x 1.0 + 3 * 2.0 = 12
    // 12 x 0.10 = 1.20
    assert(serviceCharge(listOfItems2) === 1.20)

    // includes any hot food, should return 20% of the total bill with a maximum £20 service charge
    val listOfItems3: List[ItemCaseClass] = List(coffee, coffee, coffee, coffee, coffee, coffee, steakSandwich, steakSandwich, steakSandwich)
    // 6 x 1.0 + 3 x 4.5 = 19.5
    // 19.5 x 0.2 = 3.9
    assert(serviceCharge(listOfItems3) === 3.9)

    // Testing maximum £20 service charge
    val listOfItems4: List[ItemCaseClass] = List(steakSandwich)
    // 4.50 x 23 = 103.5
    // 103.5 x 0.2 = 20.7 -> 20
    val multipliedList: List[ItemCaseClass] = listOfItems4.map(item =>
      item.copy(price = item.price * 24))
    assert(serviceCharge(multipliedList) === 20)

    // Includes any premium items, should return 25% of the total bill with a maximum of £40 service charge
    val listOfItems5: List[ItemCaseClass] = List(coffee, coffee, coffee, coffee, coffee, coffee, lobster)
    // 6 x 1.0 + 1 x 25.0 = 31
    // 31 x 0.25 = 7.75
    assert(serviceCharge(listOfItems5) == 7.75)

    // Service charge higher than £40 returns £40
    val listOfItems6: List[ItemCaseClass] = List(coffee, coffee, coffee, coffee, coffee, coffee, lobster, lobster, lobster, lobster, lobster, lobster, lobster)
    // 6 x 1.0 + 7 x 25.0 = 181
    // 181 x 0.25 = 45.25 -> 40
    assert(serviceCharge(listOfItems6) == 40)


  }


  "costCalculation" should "calculate and return the total cost" in {
    // No service charge -> 0%, No loyalty
    val listOfItem1: List[ItemCaseClass] = List(cocaCola, coffee)
    // 0.5 + 1.0 = 1.5
    assert(costCalculation(listOfItem1, 0, 0, 0) == 1.5)

    // Food service charge -> 10% -> , No loyalty
    val listOfItems2: List[ItemCaseClass] = List(cocaCola, coffee, cheeseSandwich)
    // 0.5 + 1.0 + 2.0 = 3.5
    // 3.5 x 0.10 = 0.35
    // 3.5 + 0.35 = 3.85
    assert(costCalculation(listOfItems2, 0, 0, 0.35) == 3.85)

    // Hot food service charge, No loyalty
    val listOfItems3: List[ItemCaseClass] = List(cocaCola, coffee, cheeseSandwich, steakSandwich)
    // 0.5 + 1.0 + 2.0 + 4.50 = 8.0
    // 8.0 x 0.20 = 1.60
    // 8.0 + 1.60 = 9.60
    assert(costCalculation(listOfItems3, 0, 0, 1.60) == 9.60)

    // Premium item included, No loyalty
    val listOfItems4: List[ItemCaseClass] = List(cocaCola, coffee, cheeseSandwich, steakSandwich, lobster)
    // 0.5 + 1.0 + 2.0 + 4.50 + 25 = 33
    // 33 x 0.25 = 8.25
    // 33 + 8.25 = 9.60
    assert(costCalculation(listOfItems4, 0, 0, 8.25) == 41.25)

    // No service charge -> 0%, Loyalty
    // 0.5 + 1.0 = 1.5
    // 5 stars
    // 5 x 0.025 = 0.125
    // 1.5 x 0.125 = 0.1875
    // 1.5 - 0.1875 = 1.31
    assert(costCalculation(listOfItem1, 0.125, 0.1875, 0) == 1.31)

    // Food service charge -> 10% -> , loyalty
    // 0.5 + 1.0 + 2.0 = 3.5
    // 5stars
    // loyalty percentage: 5 x 0.025 = 0.125
    // loyalty amount: 3.5 x 0.125 = 0.4375
    // service charge: 3.5 x 0.10 = 0.35
    // total: 3.5 - 0.4375 + 0.35 = 3.85
    assert(costCalculation(listOfItems2, 0.125, 0.4375, 0.35) == 3.41)
  }
}
