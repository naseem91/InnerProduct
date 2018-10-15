import org.scalatest.{FlatSpec, Matchers}

class Test extends FlatSpec with Matchers {

  "Lenght of list1" should "be equals lenght of list2" in {
    assert(InnerProductExamples.list1.length == InnerProductExamples.list2.length)
  }

  it should "consistent in size " in {
    a[java.lang.AssertionError] should be thrownBy {
      assert(InnerProductExamples.list1.length != InnerProductExamples.list2.length)
    }

  }
}