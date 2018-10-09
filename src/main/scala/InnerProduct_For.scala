object InnerProduct_For {
  def main(args: Array[String]): Unit = {
    //Example
    val list1 = List(1, 2, 3, 4)
    val list2 = List(4, 5, 6, 7)
    val t0 = System.nanoTime()
    val res = innerProduct[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)+ "ns")
    println("Result = "+res)
  }
  def innerProduct[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    //tms & pls are generic functions
    // while tms can be * and pls can be + they don't need to be
    var acc = zero
    val n = l1.length
    assert(l2.length == n) //throws assert error if the lists are not of equal length
    //alternative 1 trim the longer list
    //alternative 2 tms with 'oneA' or 'oneB'
    //clearly none of these three options are very good
    //If type checking could impose a restriction on the length of lists that would work, but we don't have that feature in most languages.
    //However, working with a single list of tuples solves this problem
    // your type becomes (list:Seq[(A,B)]) instead of (l1:Seq[A]) (l2:Seq[B])
    //This might have been a better definition. The question of what to do with uneven lists is solved already in the contruction of the list, and the type check is all that is needed to avoid runtime errors.
    //It also lets you generalise tms further tms:A=>B where A,B can both be tuples. for example: A could be a 3tuple (X,Y,Z)

    for (i <- 0 until n) {
      acc = pls(acc, tms(l1(i), l2(i)))
    }
    acc
  }
}