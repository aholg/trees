case class BinaryTree(value: Int, var left: Option[BinaryTree] = None, var right: Option[BinaryTree] = None, var parent: Option[BinaryTree] = None) {
  def exists(queryNumber: Int): Boolean = {
    if (queryNumber == value) {
      true
    } else if (queryNumber < value) {
      left.map(_.exists(queryNumber)).getOrElse(false)
    } else {
      right.map(_.exists(queryNumber)).getOrElse(false)
    }
  }

  def insert(newValue: Int): Boolean = {
    if (newValue == value) {
      false
    } else if (newValue < value) {
      left match {
        case None => {
          this.left = Some(BinaryTree(newValue, parent = Some(this)))
          true
        }
        case Some(node) => node.insert(newValue)
      }
    } else {
      right match {
        case None => {
          this.right = Some(BinaryTree(newValue, parent = Some(this)))
          true
        }
        case Some(node) => node.insert(newValue)
      }
    }
  }

  def remove(valueToRemove: Int): Boolean = {
    if (this.value != valueToRemove) {
      if (valueToRemove < this.value) {
        this.left.map(_.remove(valueToRemove)).getOrElse(false)
      } else {
        this.right.map(_.remove(valueToRemove)).getOrElse(false)
      }
    } else {
      (this.left, this.right) match {
        case (None, None) => {
          this.parent.map { parent =>
            if (parent.left.exists(l => this.value == l.value)) {
              parent.left = None
            } else {
              parent.right = None
            }
          }
          true
        }
        case _ => false
      }
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case t: BinaryTree => (this.value.equals(t.value) && this.left==t.left && this.right == t.right)
      case _ => false
    }
  }

  override def toString(): String = {
    s"BinaryTree(value=$value, left=$left, right=$right)"
  }
}




