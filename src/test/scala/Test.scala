import org.scalatest.{FlatSpec, Matchers}

class Test extends FlatSpec with Matchers {

  "Lenght of list1" should "be equals lenght of list2" in {
    assert(InnerProduct.list1.length == InnerProduct.list2.length)
  }

  it should "consistent in size " in {
    a[java.lang.AssertionError] should be thrownBy {
      assert(InnerProduct.list1.length != InnerProduct.list2.length)
    }

    //  "Adding two numbers " should "be" in {
    //    InnerProduct2.addInt(4,2) should be (6)
    //  }
  }
}