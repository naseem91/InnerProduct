
object InnerProductExamples {

  import InnerProduct._

  val list1 = List(1,8,5)
  val list2 = List(4, 7,9)

  var t0 = System.nanoTime()
  val res = innerProduct_For[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
  var t1 = System.nanoTime()
  println("Result using for = "+res + (t1 - t0) + "ns")

  t0 = System.nanoTime()
  val res2 = innerProduct_Next[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
  t1 = System.nanoTime()
  println("Result using next = "+res2 + (t1 - t0) + "ns")

  t0 = System.nanoTime()
  val res3 = innerProduct_While[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
  t1 = System.nanoTime()
  println("Result using while = "+res3 + (t1 - t0) + "ns")

  t0 = System.nanoTime()
  val res4 = innerProduct_Map[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
  t1 = System.nanoTime()
  println("Result using map = "+res4 + (t1 - t0) + "ns")

  t0 = System.nanoTime()
  val res5 = innerProduct_FoldLeft[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
  t1 = System.nanoTime()
  println("Result using fold left = "+res5 + (t1 - t0) + "ns")

  t0 = System.nanoTime()
  val res6 = innerProduct_Foreach[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
  t1 = System.nanoTime()
  println("Result using foreach = "+res6 + (t1 - t0) + "ns")

  t0 = System.nanoTime()
  val res7 = innerProduct_Recursion[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
  t1 = System.nanoTime()
  println("Result using recursion = "+res7 + (t1 - t0) + "ns")
}
