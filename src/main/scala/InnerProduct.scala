object InnerProduct {

  //Inner product using for
  def innerProduct_For[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    var acc = zero
    val n = l1.length
    assert(l2.length == n)
    for (i <- 0 until n) {
      acc = pls(acc, tms(l1(i), l2(i)))
    }
    acc
  }
  //Inner product using Next
  def innerProduct_Next[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    var acc = zero
    val n = l1.length
    assert(l2.length == n)
    val it1 = l1.iterator
    val it2 = l2.iterator
    while (it1.hasNext && it2.hasNext) {
        acc = pls(acc, tms(it1.next(),it2.next()))
    }
    acc
  }
  //Inner product using while
  def innerProduct_While[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    var acc = zero
    val n = l1.length
    assert(l2.length == n)
    var i =0
    while( i < n) {
      acc = pls(acc, tms(l1(i), l2(i)))
      i=i+1
    }
    acc
  }
  //Inner product using map
  def innerProduct_Map[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    var acc = zero
    val n = l1.length
    assert(l2.length == n)
    val compoundList:Seq[(A,B)]=l1.zip(l2)
    compoundList.map{tub => acc=pls(acc, tms(tub._1 ,tub._2))}
    acc
  }
  //Inner product using fold left
  def innerProduct_FoldLeft[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    var res =zero
    val n = l1.length
    assert(l2.length == n)
    var templist:List[C] = List()
    for (i <- 0 until n) {
      templist=tms(l1(i),l2(i))::templist
    }
    res = templist.foldLeft(zero){pls}
    res
  }
  //Inner product using Foreach
  def innerProduct_Foreach[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    var acc = zero
    val n = l1.length
    assert(l2.length == n)
    val compoundList:Seq[(A,B)]=l1.zip(l2)
    compoundList.foreach{ tub => acc=pls(acc, tms(tub._1 ,tub._2))}
    acc
  }
  //Inner product recursion
  def innerProduct_Recursion[A, B, C, D](l1: Seq[A])(l2: Seq[B])(tms: (A, B) => C)(pls: (D, C) => D)(zero: D): D = {
    var acc = zero
    val n = l1.length
    assert(l2.length == n)
    def start(acc:D,n:Int): D ={
      if(n == 0)
        acc
      else
        start(pls(acc, tms(l1(n-1), l2(n-1))),n-1)
    }
    start(zero,n)
  }

  def main(args: Array[String]): Unit = {}
}


