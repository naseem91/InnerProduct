object InnerProduct {
  def main(args: Array[String]): Unit = {
    val list1:List[Double]=List(1,2,3)
    val list2:List[Double] =List(1,2,3)
    println(list1 + " . " + list2)
    println(dotproduct(list1,list2))
  }
  def dotproduct(listA:List[Double],listB:List[Double]):Double ={
    var tms:List[Double]=times(listA,listB)
    pls(tms)
  }
  def times(list1:List[Double],list2:List[Double]):List[Double]=list1.zip(list2).map { case (a, b) => a * b }
  def pls(list:List[Double])=list.fold(0.0)(_+_)
}


