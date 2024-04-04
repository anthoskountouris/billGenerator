trait MenuItem {
  var cost: BigDecimal = BigDecimal(0)

  def printMenu():Unit
//  def itemPrice(item:ItemCaseClass):BigDecimal
  def costCalculation(listOfItems:List[ItemCaseClass]): BigDecimal
  def numberToItem(num:String):Either[Option[ItemCaseClass], Boolean]

}
