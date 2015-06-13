package corpnet

object Logger {
  var logOutputHandlers = List.empty[(String) => Unit]

  def registerHandler(handler: (String) => Unit) = {
    logOutputHandlers = handler :: logOutputHandlers
  }

  def error(text: String): Unit = logOutputHandlers.foreach(_("ERROR: " + text))

  def debug(text: String): Unit = logOutputHandlers.foreach(_("DEBUG: " + text))

  def info(text: String): Unit = logOutputHandlers.foreach(_("INFO: " + text))

  def warn(text: String): Unit = logOutputHandlers.foreach(_("WARN: " + text))
}
