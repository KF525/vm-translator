package kfulton.nand2tetris2.parser

object VMCommand {
  def fromString(s: String): Option[VMCommand] =
    Seq(Add, Subtract, Negative, Equal, GreaterThan, LessThan, And, Or, Not).find(_.name == s)
}

//trait VMCommand2
//case class SimpleVMCommand(commandString: String)

sealed abstract class VMCommand(val name:String)
case class Push(segment: VMSegment, location: Int) extends VMCommand("push")
case class Pop(segment: VMSegment, location: Int) extends VMCommand("pop")
case object Add extends VMCommand("add")
case object Subtract extends VMCommand("sub")
case object Negative extends VMCommand("neg")
case object Equal extends VMCommand("eq")
case object GreaterThan extends VMCommand("gt")
case object LessThan extends VMCommand("lt")
case object And extends VMCommand("and")
case object Or extends VMCommand("or")
case object Not extends VMCommand("not")
case class GoTo(variable: String) extends VMCommand("goto")
case class IfGoTo(variable: String) extends VMCommand("if-goto")
case class Label(variable: String) extends VMCommand("label")
case object FunctionReturn extends VMCommand("return")

object VMSegment {
  def fromString(s: String): Option[VMSegment] =
    Seq(Static, Local, This, That, Constant, Pointer, Temp, Argument).find(_.toString.equalsIgnoreCase(s))
}

sealed abstract class VMSegment(val name: String)
case object Static extends VMSegment("STATIC")
case object Local extends VMSegment("LCL")
case object This extends VMSegment("THIS")
case object That extends VMSegment("THAT")
case object Constant extends VMSegment("CONSTANT")
case object Pointer extends VMSegment("POINTER")
case object Temp extends VMSegment("TEMP")
case object Argument extends VMSegment("ARG")