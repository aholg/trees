package binarytrees

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.{Future, ExecutionContext => EC}

class ConcurrentNodeTest extends FunSuite with Matchers with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit override val patienceConfig =
    PatienceConfig(timeout = Span(6, Seconds), interval = Span(5, Millis))

  test("Should insert numbers in the correct order") {
    val tree = ConcurrentNode(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree shouldEqual ConcurrentNode(10, left = Some(ConcurrentNode(8)), right = Some(ConcurrentNode(11, None, Some(ConcurrentNode(12)))))
  }

  test("Should not insert if number already exists") {
    val tree = ConcurrentNode(10, None, None)

    tree.insert(10).futureValue shouldEqual false
    tree.insert(11).futureValue shouldEqual true
  }

  test("Should be able to find a number") {
    val tree = ConcurrentNode(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.exists(11) shouldEqual true
  }

  test("Should return false if number was not found") {
    val tree = ConcurrentNode(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.exists(20) shouldEqual false
  }

  test("Should return false if number was not removed") {
    val tree = ConcurrentNode(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.remove(20).futureValue shouldEqual false
  }

  test("Should return true if number was removed and tree should be reordered") {
    val tree = ConcurrentNode(10, None, None)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    whenReady(tree.remove(8)){ result =>
      result shouldEqual true
    }
    tree shouldEqual ConcurrentNode(10, right = Some(ConcurrentNode(11, None, Some(ConcurrentNode(12)))))
  }

  test("Should insert correctly when removal is in progress") {
    val tree = ConcurrentNode(7, None, None)

    tree.insert(3)
    tree.insert(1)
    tree.insert(5)
    tree.insert(4)
    tree.insert(6)

    tree.remove(3 )
    whenReady(tree.insert(3)){ result =>
      result shouldEqual true
    }
    tree shouldEqual ConcurrentNode(7, left = Some(ConcurrentNode(4, Some(ConcurrentNode(1, right = Some(ConcurrentNode(3)))), Some(ConcurrentNode(5, right = Some(ConcurrentNode(6)))))))
  }
}
