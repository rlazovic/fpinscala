package chapter3

import org.scalatest._


class ListTest extends FlatSpec with Matchers {
  val list5 = List(1, 2, 3, 4, 5)

  "A List" should "be initialized as empty list" in {
    val emptyList = List()
    emptyList should be(Nil)
  }

  it should "be initialized as list" in {
    val list = List(1, 2, 3, 4)
    list should be(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
  }

  it should "implement tail on list" in {
    val list = List(1,2,3)
    List.tail(list) shouldBe(List(2,3))
  }

  it should "implement tail on empty list" in {
    val emptyList = List()
    List.tail(emptyList) shouldBe(Nil)
  }

  it should "implement setHead on list" in {
    List.setHead(5, List(1,2,3)) shouldBe(List(5,1,2,3))
  }

  it should "implement setHead on empty list" in {
    List.setHead(3, List()) shouldBe(List(3))
  }

  it should "implement sum" in {
    List.sum(list5) shouldBe(15)
  }

  it should "implement product" in {
    List.product(List(1.0,2,3,4,5)) shouldBe(120)
  }

  it should "calculate length of empty list as 0" in {
    List.length(List()) shouldBe 0
  }

  it should "calculate length of list" in {
    List.length(list5) shouldBe 5
  }

  it should "reverse empty list" in {
    List.reverse(List()) shouldBe List()
  }
  it should "reverse a list" in {
    List.reverse(list5) shouldBe List(5,4,3,2,1)
  }
}
