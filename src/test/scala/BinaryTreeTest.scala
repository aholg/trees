import org.scalatest.{FunSuite, Matchers}

class BinaryTreeTest extends FunSuite with Matchers {

  test("Should insert numbers in the correct order") {
    val tree = BinaryTree(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree shouldEqual BinaryTree(10, left = Some(BinaryTree(8)), right = Some(BinaryTree(11, None, Some(BinaryTree(12)))))
  }

  test("Should not insert if number already exists") {
    val tree = BinaryTree(10, None, None)

    tree.insert(10) shouldEqual false
    tree.insert(11) shouldEqual true
  }

  test("Should be able to find a number") {
    val tree = BinaryTree(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.exists(11) shouldEqual true
  }

  test("Should return false if number was not found") {
    val tree = BinaryTree(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.exists(20) shouldEqual false
  }

  test("Should return false if number was not removed") {
    val tree = BinaryTree(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.remove(20) shouldEqual false
  }

  test("Should return true if number was removed and tree should be reordered") {
    val tree = BinaryTree(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.remove(8) shouldEqual true
    tree shouldEqual BinaryTree(10, right = Some(BinaryTree(11, None, Some(BinaryTree(12)))))
  }
}
