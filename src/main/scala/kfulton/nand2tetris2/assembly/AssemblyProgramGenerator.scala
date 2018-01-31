package kfulton.nand2tetris2.assembly

sealed abstract class Register(val name: Option[String])
case class VariableRegister(s: String) extends Register(None)
case class NumberRegister(i: Int) extends Register(None)
case object StackPointer extends Register(Some("@SP"))
case object ArgumentPointer extends Register(Some("@ARG"))
case object LocalPointer extends Register(Some("@LCL"))
case object ThisPointer extends Register(Some("@THIS"))
case object ThatPointer extends Register(Some("@THAT"))
case object MRegister extends Register(Some("M"))
case object DRegister extends Register(Some("D"))
case object ARegister extends Register(Some("A"))

case class ConstantExp(i: Int)
case class RegisterExp(r: Register)

trait Expression
case class EqualExpression(a: Either[RegisterExp, ConstantExp]) extends Expression
case class AddExpression(a: Either[RegisterExp, ConstantExp], b: Either[RegisterExp, ConstantExp]) extends Expression
case class SubtractExpression(a: Either[RegisterExp, ConstantExp], b: Either[RegisterExp, ConstantExp]) extends Expression

trait AssemblyCommand
case class ValueAssignment(const: ConstantExp, expression: Expression) extends AssemblyCommand
case class RegisterAssignment(reg: RegisterExp, expression: Expression) extends AssemblyCommand
case class JumpNotEqual(a: Either[RegisterExp, ConstantExp]) extends AssemblyCommand
case class JumpEqual(a: Either[RegisterExp, ConstantExp]) extends AssemblyCommand
case class JumpGreaterThan(a: Either[RegisterExp, ConstantExp]) extends AssemblyCommand
case class JumpGreaterThanOrEqual(a: Either[RegisterExp, ConstantExp]) extends AssemblyCommand
case class JumpLessThan(a: Either[RegisterExp, ConstantExp]) extends AssemblyCommand
case class JumpLessThanOrEqual(a: Either[RegisterExp, ConstantExp]) extends AssemblyCommand
case class Jump(a: Either[RegisterExp, ConstantExp]) extends AssemblyCommand
case class Loop(name: String) extends AssemblyCommand

class AssemblyProgramGenerator {

  def generateAssemblyCode(command: AssemblyCommand) = command match {
    case jne: JumpNotEqual =>
      val exp = getExp(jne.a); s"$exp; JNE"
    case jeq: JumpEqual =>
      val exp = getExp(jeq.a); s"$exp; JEQ"
    case jgt: JumpGreaterThan =>
      val exp = getExp(jgt.a); s"$exp; JGT"
    case jge: JumpGreaterThanOrEqual =>
      val exp = getExp(jge.a); s"$exp; JGE"
    case jlt: JumpLessThan =>
      val exp = getExp(jlt.a); s"$exp; JLT"
    case jle: JumpLessThanOrEqual =>
      val exp = getExp(jle.a); s"$exp; JLE"
    case jmp: Jump =>
      val exp = getExp(jmp.a); s"$exp; JMP"
    case va: ValueAssignment =>
      s"@${va.const.i} = ${getExpression(va.expression)}"
    case ra: RegisterAssignment =>
      val register = getRegister(ra.reg.r)
      s"$register = ${getExpression(ra.expression)}"
  }

  def getRegister(r: Register) = r match {
    case variable: VariableRegister => s"@${variable.s}"
    case num: NumberRegister => s"@${num.i}"
    case x: Register => x.name.get
  }

  private def getExp(a: Either[RegisterExp, ConstantExp]): String =  a match {
    case Left(reg) => reg.r.name.getOrElse("")
    case Right(const) => const.i.toString
  }

  def getExpression(exp: Expression): String = exp match {
    case equal: EqualExpression => getExp(equal.a).toString
    case add: AddExpression => s"${getExp(add.a)} + ${getExp(add.b)}"
    case sub: SubtractExpression => s"${getExp(sub.a)} - ${getExp(sub.b)}"
  }
}