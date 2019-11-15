package binarytrees

import org.scalatest.{FunSuite, Matchers}

class NodeTest extends FunSuite with Matchers {

  test("Should insert numbers in the correct order") {
    val tree = Node(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree shouldEqual Node(10, left = Some(Node(8)), right = Some(Node(11, None, Some(Node(12)))))
  }

  test("Should not insert if number already exists") {
    val tree = Node(10, None, None)

    tree.insert(10) shouldEqual false
    tree.insert(11) shouldEqual true
  }

  test("Should be able to find a number") {
    val tree = Node(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.exists(11) shouldEqual true
  }

  test("Should return false if number was not found") {
    val tree = Node(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.exists(20) shouldEqual false
  }

  test("Should return false if number was not removed") {
    val tree = Node(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.remove(20) shouldEqual false
  }

  test("Should return true if number was removed and tree should be reordered") {
    val tree = Node(10, None, None)

    tree.insert(8)
    tree.insert(6)
    tree.insert(9)
    tree.insert(11)
    tree.insert(12)

    tree.remove(8 ) shouldEqual true
    tree shouldEqual Node(10, left = Some(Node(9, left = Some(Node(6)))), right = Some(Node(11, None, Some(Node(12)))))
  }

  test("Should remove value with multiple children and reorder tree") {
    val tree = Node(7, None, None)

    tree.insert(3)
    tree.insert(1)
    tree.insert(5)
    tree.insert(4)
    tree.insert(6)

    tree.remove(3 ) shouldEqual true
    tree shouldEqual Node(7, left = Some(Node(4, Some(Node(1)), Some(Node(5, right = Some(Node(6)))))))
  }
}
