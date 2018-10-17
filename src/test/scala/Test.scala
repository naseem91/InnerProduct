import org.scalatest.FunSpec
class Test extends FunSpec {
  describe("The Sequence 1 :from " +InnerProductExamples.list1) {
    describe("(when empty)") { // All tests within these curly braces are about "A Set (when empty)"
      it("should have size 0") { // Here, 'it' refers to "A Set (when empty)". The full name
        assert(InnerProductExamples.list1.size == 0) // of this test is: "A Set (when empty) should have size 0"
      }
      it("should produce NoSuchElementException when head is invoked") { // Define another test
        intercept[NoSuchElementException] {
          InnerProductExamples.list1.head
        }
      }

    }
  }
  describe("(when in consistent )") {
    it("should be non-empty") {
      assert(InnerProductExamples.list1.size != 0 && InnerProductExamples.list2.size != 0)
    }
    it("should have the same size") {
      assert(InnerProductExamples.list1.size == InnerProductExamples.list2.size)
    }
  }
}
