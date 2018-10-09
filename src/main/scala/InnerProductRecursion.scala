object InnerProductRecursion {
  def main(args: Array[String]): Unit = {
    //Example
    val list1 = List(1, 2, 3, 4)
    val list2 = List(4, 5, 6, 7)
    val t0 = System.nanoTime()
    val res = innerProduct[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    println("Result = "+res)
  }
  def innerProduct[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    //tms & pls are generic functions
    // while tms can be * and pls can be + they don't need to be
    var acc = zero
    val n = l1.length
    assert(l2.length == n) //throws assert error if the lists are not of equal length
    def start(acc:D,n:Int): D ={
      if(n == 0)
        acc
      else
        start(pls(acc, tms(l1(n-1), l2(n-1))),n-1)
    }
    start(zero,n)
  }
}

