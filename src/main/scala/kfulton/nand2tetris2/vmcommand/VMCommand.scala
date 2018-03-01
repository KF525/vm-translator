package kfulton.nand2tetris2.vmcommand

object VMCommand {

  def fromString(s: String): Option[VMCommand] =
    Seq(Add, Subtract, Negative, Equal, GreaterThan, LessThan, And, Or, Not).find(_.name == s)
}

trait VMCommand { val name:String }
case class Push(segment: VMSegment, location: Int) extends VMCommand { val name:String = "push" }
case class Pop(segment: VMSegment, location: Int) extends VMCommand { val name:String = "pop" }
case object Add extends VMCommand { val name:String = "add" }
case object Subtract extends VMCommand { val name:String = "sub" }
case object Negative extends VMCommand { val name:String = "neg" }
case object Equal extends VMCommand { val name:String = "eq" }
case object GreaterThan extends VMCommand { val name:String = "gt" }
case object LessThan extends VMCommand { val name:String = "lt" }
case object And extends VMCommand { val name:String = "and" }
case object Or extends VMCommand { val name:String = "or" }
case object Not extends VMCommand { val name:String = "not" }

object VMSegment {
  def fromString(s: String): Option[VMSegment] =
    Seq(Static, Local, This, That, Constant, Pointer, Temp, Argument).find(_.toString.equalsIgnoreCase(s))
}

sealed abstract class VMSegment
case object Static extends VMSegment
case object Local extends VMSegment
case object This extends VMSegment
case object That extends VMSegment
case object Constant extends VMSegment
case object Pointer extends VMSegment
case object Temp extends VMSegment
case object Argument extends VMSegment