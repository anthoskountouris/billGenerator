trait MenuItem {
  var cost: BigDecimal = BigDecimal(0)
  def printMenu():Unit
  def serviceCharge(listOfItems:List[ItemCaseClass]): BigDecimal
  def costCalculation(listOfItems:List[ItemCaseClass], loyaltyPerc:BigDecimal, loyaltyAmount:BigDecimal, serviceCharge:BigDecimal): BigDecimal
  def numberToItem(num:String):Either[Option[ItemCaseClass], Boolean]
  def printBasket(listOfItems:List[ItemCaseClass]): Unit
  def askingForLoyalty(listOfItems:List[ItemCaseClass],hasLoyalty:String, startsNo:Int):(BigDecimal,BigDecimal)}
