object InnerProduct_FoldLeft {
  def main(args: Array[String]): Unit = {
    //Example
    val list1 = List(1, 2, 3, 4)
    val list2 = List(4, 5, 6, 7)
    val innerResult = innerProduct[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    println("Result = "+innerResult)
  }
  def innerProduct[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    //tms & pls are generic functions
    // while tms can be * and pls can be + they don't need to be
    var res =zero
    val n = l1.length
    assert(l2.length == n) //throws assert error if the lists are not of equal length
    var templist:List[C] = List()
    for (i <- 0 until n) {
      templist=tms(l1(i),l2(i))::templist
    }
    res = templist.foldLeft(zero){pls}
    res
  }
}
