
object InnerProductExamples {

  import InnerProduct._
  val list1 = 1 to 4
  val list2 = 1 to 4
  def main(args: Array[String]): Unit = {

    var times = collection.mutable.Map[String,Double]()

    var t0 = System.nanoTime()
    val res = innerProduct_For[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    var t1 = System.nanoTime()
    times += ("for" -> (t1 - t0)/1000000.0)
    println("Result using for = "+res +" during " + (t1 - t0)/1000000.0 + " s")
    t0 = System.nanoTime()
    val res2 = innerProduct_Next[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    t1 = System.nanoTime()
    println("Result using next = "+res2 + " during " + (t1 - t0)/1000000.0 + " s")
    times += ("next" -> (t1 - t0)/1000000.0)
    t0 = System.nanoTime()
    val res3 = innerProduct_While[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    t1 = System.nanoTime()
    println("Result using while = "+res3 + " during " + (t1 - t0)/1000000.0 + " s")
    times += ("while" -> (t1 - t0)/1000000.0)
    t0 = System.nanoTime()
    val res4 = innerProduct_Map[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    t1 = System.nanoTime()
    println("Result using map = "+res4  + " during " +(t1 - t0)/1000000.0 + " s")
    times += ("map" -> (t1 - t0)/1000000.0)
    t0 = System.nanoTime()
    val res5 = innerProduct_FoldLeft[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    t1 = System.nanoTime()
    println("Result using fold left = "+res5  + " during " + (t1 - t0)/1000000.0 + " s")
    times += ("fold-left" -> (t1 - t0)/1000000.0)
    t0 = System.nanoTime()
    val res6 = innerProduct_Foreach[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    t1 = System.nanoTime()
    println("Result using foreach = "+res6  + " during " + (t1 - t0)/1000000.0 + " s")
    times += ("forEach" -> (t1 - t0)/1000000.0)
    t0 = System.nanoTime()
    val res7 = innerProduct_Recursion[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    t1 = System.nanoTime()
    println("Result using recursion = "+res7  + " during " + (t1 - t0)/1000000.0 + " s")
    times += ("recursion" -> (t1 - t0)/1000000.0)

    t0 = System.nanoTime()
    val res8 = innerProduct_HeadTail[Int, Int, Int, Int](list1)(list2)(_ * _)(_ + _)(0)
    t1 = System.nanoTime()
    println("Result using head and tail = "+res8  + " during " + (t1 - t0)/1000000.0 + " s")
    times += ("Head and tail" -> (t1 - t0)/1000000.0)
    println("*************************")
    // Gets the way which calculate the result of inner product in best performance
    val min = times.values.min
    var way : String = ""
    times.keys.foreach{ x => if (min == times.get(x).get) way = x  }
    println("The minimum time is "  + min + " with " + way )
    // Gets the way which calculate the result of inner product in bad performance
    val max = times.values.max
    times.keys.foreach{ x => if (max == times.get(x).get) way = x  }
    println("The maximum time is "  + max + " with " + way )
  }

}