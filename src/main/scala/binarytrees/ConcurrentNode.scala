package binarytrees

import scala.concurrent.Future

case class ConcurrentNode(var value: Int, var left: Option[ConcurrentNode] = None, var right: Option[ConcurrentNode] = None, var parent: Option[ConcurrentNode] = None) {
  def exists(queryNumber: Int): Boolean = {
    if (queryNumber == value) {
      true
    } else if (queryNumber < value) {
      left.map(_.exists(queryNumber)).getOrElse(false)
    } else {
      right.map(_.exists(queryNumber)).getOrElse(false)
    }
  }

  def insert(newValue: Int): Future[Boolean] = {
    if (newValue == value) {
      Future.successful(false)
    } else if (newValue < value) {
      left match {
        case None => {
          this.left = Some(ConcurrentNode(newValue, parent = Some(this)))
          Future.successful(true)
        }
        case Some(node) => node.insert(newValue)
      }
    } else {
      right match {
        case None => {
          this.right = Some(ConcurrentNode(newValue, parent = Some(this)))
          Future.successful(true)
        }
        case Some(node) => node.insert(newValue)
      }
    }
  }

  def remove(valueToRemove: Int): Boolean = {
    if (this.value == valueToRemove) {
      (this.right) match {
        case Some(r) =>
          this.value = findSmallestValue(r)
          true
        case None =>
          false
      }
    }
    else {
      if (valueToRemove < this.value) {
        this.left.map(_.remove(valueToRemove)).getOrElse(false)
      } else {
        this.right.map(_.remove(valueToRemove)).getOrElse(false)
      }
    }
  }

  def findSmallestValue(node: ConcurrentNode): Int = {
    if (node.left.isEmpty) {
      node.parent.map { p =>
        if(p.left.exists(_.value == node.value)) p.left = None
        else p.right = None
      }
      node.value
    }
    else findSmallestValue(node.left.get)
  }


  override def equals(obj: Any): Boolean = {
    obj match {
      case t: ConcurrentNode => (this.value.equals(t.value) && this.left == t.left && this.right == t.right)
      case _ => false
    }
  }

  override def toString(): String = {
    s"ConcurrentTreeWithLocks(value=$value, left=$left, right=$right)"
  }
}
