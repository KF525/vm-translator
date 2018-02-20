package kfulton.nand2tetris2.vmcommand

trait VMCommand
case class Push(segment: Segment, location: Int) extends VMCommand
case class Pop(segment: Segment, location: Int) extends VMCommand
case object Add extends VMCommand
case object Subtract extends VMCommand
case object Negative extends VMCommand
case object Equal extends VMCommand
case object GreaterThan extends VMCommand
case object LessThan extends VMCommand
case object And extends VMCommand
case object Or extends VMCommand


sealed abstract class Segment(name: String)
case object StackPointer extends Segment("sp")
case object Local extends Segment("local")
case object This extends Segment("this")
case object That extends Segment("that")
case object Constant extends Segment("constant")
case object Pointer extends Segment("pointer")
case object Temp extends Segment("temp")