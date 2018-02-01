package kfulton.nand2tetris2.assembly

case class Value(registerOrConstant: Either[RegisterExp, ConstantExp])

sealed abstract class Register
case class NameRegister(s: String) extends Register
case class NumberRegister(i: Int) extends Register
case class SpecialRegister(s: String) extends Register

case class ConstantExp(i: Int)
case class RegisterExp(r: Register)

trait Expression
case class AssignmentExpression(a: Value) extends Expression
case class AddExpression(a: Value, b: Value) extends Expression
case class SubtractExpression(a: Value, b: Value) extends Expression

trait AssemblyCommand
case class RegisterAssignment(reg: RegisterExp, expression: Expression) extends AssemblyCommand
case class JumpNotEqual(a: Value) extends AssemblyCommand
case class JumpEqual(a: Value) extends AssemblyCommand
case class JumpGreaterThan(a: Value) extends AssemblyCommand
case class JumpGreaterThanOrEqual(a: Value) extends AssemblyCommand
case class JumpLessThan(a: Value) extends AssemblyCommand
case class JumpLessThanOrEqual(a: Value) extends AssemblyCommand
case class Jump(a: Value) extends AssemblyCommand
case class Loop(name: String) extends AssemblyCommand

class AssemblyProgramGenerator {

  def generateAssemblyCode(command: AssemblyCommand) = command match {
    case jne: JumpNotEqual =>
      val comparisonValue = getValue(jne.a); s"$comparisonValue; JNE"
    case jeq: JumpEqual =>
      val comparisonValue = getValue(jeq.a); s"$comparisonValue; JEQ"
    case jgt: JumpGreaterThan =>
      val comparisonValue = getValue(jgt.a); s"$comparisonValue; JGT"
    case jge: JumpGreaterThanOrEqual =>
      val comparisonValue = getValue(jge.a); s"$comparisonValue; JGE"
    case jlt: JumpLessThan =>
      val comparisonValue = getValue(jlt.a); s"$comparisonValue; JLT"
    case jle: JumpLessThanOrEqual =>
      val comparisonValue = getValue(jle.a); s"$comparisonValue; JLE"
    case jmp: Jump =>
      val comparisonValue = getValue(jmp.a); s"$comparisonValue; JMP"
    case assignment: RegisterAssignment =>
      val register = getRegister(assignment.reg.r)
      s"$register = ${getExpression(assignment.expression)}"
  }

  def getRegister(r: Register): String = r match {
    case name: NameRegister => s"@${name.s}" //TODO: Could I get rid of the @ and have it in name?
    case num: NumberRegister => s"@${num.i}"
    case special: SpecialRegister => special.s
  }

  private def getValue(a:Value): String =  a.registerOrConstant match {
    case Left(reg) => getRegister(reg.r)
    case Right(const) => const.i.toString
  }

  def getExpression(exp: Expression): String = exp match {
    case equal: AssignmentExpression => getValue(equal.a).toString
    case add: AddExpression => s"${getValue(add.a)} + ${getValue(add.b)}"
    case sub: SubtractExpression => s"${getValue(sub.a)} - ${getValue(sub.b)}"
  }
}