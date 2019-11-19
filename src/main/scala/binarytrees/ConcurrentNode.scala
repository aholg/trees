package binarytrees

import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}

import scala.concurrent.{ExecutionContext, Future}

case class ConcurrentNode(var value: Int, var left: Option[ConcurrentNode] = None, var right: Option[ConcurrentNode] = None, var parent: Option[ConcurrentNode] = None)(implicit ec: ExecutionContext) {
  val lock = new ReentrantReadWriteLock()

  def exists(queryNumber: Int): Future[Boolean] = {
    if (queryNumber == value) {
      Future.successful(true)
    } else if (queryNumber < value) {
      left.map(_.exists(queryNumber)).getOrElse(Future.successful(false))
    } else {
      right.map(_.exists(queryNumber)).getOrElse(Future.successful(false))
    }
  }

  def insert(newValue: Int): Future[Boolean] = {
    println(this.lock)
    this.lock.readLock().lock()
    val eventualBoolean = if (newValue == value) {
      Future.successful(false)
    } else if (newValue < value) {
      left match {
        case None => {
          this.lock.writeLock().lock()
          this.left = Some(ConcurrentNode(newValue, parent = Some(this)))
          this.lock.writeLock().unlock()
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
    eventualBoolean.map {r =>
      this.lock.readLock().unlock()
      r
    }
  }

  def remove(valueToRemove: Int): Future[Boolean] = {
    Future {
      if (this.value == valueToRemove) {
        this.lock.writeLock().lock()
        this.parent.map(_.lock.readLock().lock())
        println(this.parent.map(_.lock))
        println("locked and loaded")
          Thread.sleep(2000)
          (this.left, this.right) match {
            case (_, Some(r)) =>
              findSmallestValue(r).map(s => this.value = s)
            case (Some(l), _) =>
              findSmallestValue(l).map(s => this.value = s)
            case _ =>
              this.parent.map { p =>
                if (p.left.exists(_.value == this.value)) p.left = None
                else p.right = None
              }
          }
        this.parent.map(_.lock.readLock().unlock())
        this.lock.writeLock().unlock()
        Future.successful(true)
      } else {
        if (valueToRemove < this.value) {
          this.left match {
            case Some(l) => l.remove(valueToRemove)
            case None => Future.successful(false)
          }
        } else {
          this.right match {
            case Some(r) => r.remove(valueToRemove)
            case None => Future.successful(false)
          }
        }
      }
    }.flatten
  }

  def findSmallestValue(node: ConcurrentNode): Future[Int] = {
    if (node.left.isEmpty) {
      node.parent.map { p =>
        if (p.left.exists(_.value == node.value)) p.left = None
        else p.right = None
      }
      Future.successful(node.value)
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
    s"ConcurrentNode(value=$value, left=$left, right=$right)"
  }
}
