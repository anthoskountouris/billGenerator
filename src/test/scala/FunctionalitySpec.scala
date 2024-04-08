import org.scalatest.flatspec.AnyFlatSpec
import Main.{askingForLoyalty, cheeseSandwich, cocaCola, coffee, costCalculation, lobster, numberToItem, steakSandwich}
import Temperature.{cold,hot}
import TypeOfItem.{drink,food}
import Category.{premium, nonPremium}


class FunctionalitySpec extends AnyFlatSpec{

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
    val listOfItems1:List[ItemCaseClass] = List(ItemCaseClass("Cola - Cold",0.5,drink,cold,nonPremium), ItemCaseClass("Coffee - Hot",1.0,drink,hot,nonPremium), ItemCaseClass("Cheese Sandwich - Cold",2.0,food,cold,nonPremium), ItemCaseClass("Steak Sandwich - Hot",4.5,food,hot,nonPremium), ItemCaseClass("Lobster",25,food,hot,premium))
    // User has loyalty card
    val yes:String = "yes"
    // 1x0 = 0
    val starsNo1:Int = 1
    assert(askingForLoyalty(listOfItems1, yes, starsNo1) === (0,0.0))
    // 3*0.025*100 = 7.5
    // 33*0.075 = 2.48
    val starsNo3:Int = 3
    assert(askingForLoyalty(listOfItems1, yes, starsNo3) === (7.5,2.48))
    // 10 * 0.025*100 = 25% => 20%
    // 33*0.20 = 6.60
    val starsNo10:Int = 10
    assert(askingForLoyalty(listOfItems1, yes, starsNo10) === (20,6.60))

    // User does not have loyalty card
    val no:String = "no"
    assert(askingForLoyalty(listOfItems1, no, 0) === (1,0.0))
  }


  "costCalculation" should "calculate and return the total cost" in {
    // No service charge -> 0%
    val listOfItems1:List[ItemCaseClass] = List(ItemCaseClass("Cola - Cold",0.5,drink,cold,nonPremium), ItemCaseClass("Coffee - Hot",1.0,drink,hot,nonPremium), ItemCaseClass("Cheese Sandwich - Cold",2.0,food,cold,nonPremium), ItemCaseClass("Steak Sandwich - Hot",4.5,food,hot,nonPremium), ItemCaseClass("Lobster",25,food,hot,premium))


    // Food service charge -> 10%
    val listOfItems2:List[ItemCaseClass] = List(ItemCaseClass("Cola - Cold",0.5,drink,cold,nonPremium), ItemCaseClass("Coffee - Hot",1.0,drink,hot,nonPremium), ItemCaseClass("Cheese Sandwich - Cold",2.0,food,cold,nonPremium), ItemCaseClass("Steak Sandwich - Hot",4.5,food,hot,nonPremium), ItemCaseClass("Lobster",25,food,hot,premium))


    // Hot food service charge -> 20% with a maximum £20 limit
    val listOfItems3:List[ItemCaseClass] = List(ItemCaseClass("Cola - Cold",0.5,drink,cold,nonPremium), ItemCaseClass("Coffee - Hot",1.0,drink,hot,nonPremium), ItemCaseClass("Cheese Sandwich - Cold",2.0,food,cold,nonPremium), ItemCaseClass("Steak Sandwich - Hot",4.5,food,hot,nonPremium), ItemCaseClass("Lobster",25,food,hot,premium))

  }
}
