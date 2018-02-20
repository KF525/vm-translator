package kfulton.nand2tetris2.vmcommand

class VMCommandMatcher {
  val PopPattern = "\\s*(pop)\\s+(\\w+)\\s+(\\d+).*".r
  val PushPattern = "\\s*(push)\\s+(\\w+)\\s+(\\d*).*".r
  val AddPattern ="\\s+(add).*".r

  def generateVMCommand(command: String): VMCommand = ???
}
