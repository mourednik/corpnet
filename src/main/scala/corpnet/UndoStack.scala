package corpnet

case class UndoStack(size: Int, stack: List[ApplicationState] = List(ApplicationState())) {
  def push(state: ApplicationState) = {
    val newStack = (state :: stack).take(size)
    UndoStack(size, newStack)
  }

  def pop() = stack match {
    case state :: Nil ⇒ None
    case state :: t   ⇒ Some(state, UndoStack(size, t))
    case Nil          ⇒ None
  }

  def head = stack.head

  def updateHead(newHead: ApplicationState) = UndoStack(size, newHead :: stack.tail)
}
