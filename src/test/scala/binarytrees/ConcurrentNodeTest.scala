package binarytrees

import java.util.concurrent.Executors

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, FunSuite, Inside, Matchers}

import scala.concurrent._
import scala.util.Random

class ConcurrentNodeTest extends FunSuite with Matchers with ScalaFutures with Inside with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit override val patienceConfig = PatienceConfig(timeout = Span(2, Seconds), interval = Span(15, Millis))

  test("Should insert numbers in the correct order") {
    val tree = ConcurrentNode(10)
    val synchronousTree = Node(10)

    tree.insert(11).map(_ => synchronousTree.insert(11))
    tree.insert(8).map(_ => synchronousTree.insert(8))

    whenReady(tree.insert(12).map(_ => synchronousTree.insert(12))) { _ =>
      tree.toString().replaceAll("Concurrent", "") shouldEqual synchronousTree.toString()
    }
  }

  test("Should insert correctly for a large amount of numbers") {
    val concurrentTree = ConcurrentNode(10)
    val synchronousTree = Node(10)

    val randomNumbers: Seq[Int] = for (i <- 0 to 1000) yield Random.nextInt

    val result = Future.traverse(randomNumbers)(r => concurrentTree.insert(r).map(_ => synchronousTree.insert(r)))

    whenReady(result) { _ =>
      concurrentTree.toString().replaceAll("Concurrent", "") shouldEqual synchronousTree.toString()
    }
  }

  test("Should not insert if number already exists") {
    val tree = ConcurrentNode(10)

    tree.insert(10).futureValue shouldEqual false
    tree.insert(11).futureValue shouldEqual true
  }

  test("Should be able to find a number") {
    val tree = ConcurrentNode(10)

    tree.insert(8)
    tree.insert(11)

    whenReady(tree.insert(12)) { _ =>
      whenReady(tree.exists(11)) {
        inside(_) {
          case result => result shouldEqual true
        }
      }
    }
  }

  test("Should return false if number was not found") {
    val tree = ConcurrentNode(10)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    whenReady(tree.exists(20)) {
      inside(_) {
        case result => result shouldEqual false
      }
    }
  }

  test("Should return false if number was not removed") {
    val tree = ConcurrentNode(10)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    tree.remove(20).futureValue shouldEqual false
  }

  test("Should return true if number was removed and tree should be reordered") {
    val tree = ConcurrentNode(10)

    tree.insert(8)
    tree.insert(11)
    tree.insert(12)

    whenReady(tree.remove(8)) { result =>
      result shouldEqual true
    }
    tree shouldEqual ConcurrentNode(10, right = Some(ConcurrentNode(11, None, Some(ConcurrentNode(12)))))
  }

  test("Should insert correctly when removal is done concurrently") {
    val tree = ConcurrentNode(7)

    tree.insert(3)
    tree.insert(1)
    tree.insert(5)
    tree.insert(4)
    tree.insert(6)

    tree.remove(3)

    val eventualBoolean = tree.insert(3)
    whenReady(eventualBoolean) { result =>
      tree shouldEqual ConcurrentNode(7, left = Some(ConcurrentNode(4, Some(ConcurrentNode(1, right = Some(ConcurrentNode(3)))), Some(ConcurrentNode(5, right = Some(ConcurrentNode(6)))))))
      result shouldEqual true
    }
  }
}
