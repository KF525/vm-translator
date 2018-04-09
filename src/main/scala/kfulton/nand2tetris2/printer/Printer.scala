package kfulton.nand2tetris2.printer

import kfulton.nand2tetris2.translator._

class Printer {

  def generateAssemblyCode(command: AssemblyCommand): String = command match {
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
    case jmp: UnconditionalJump =>
      val comparisonValue = getValue(jmp.a); s"$comparisonValue; JMP"
    case goto: GoToA => s"@${goto.name}"
    case label: LabelA => s"(${label.name})"
    case register: RegisterExp => getRegister(register.r)
    case constant: ConstantExp => s"@${constant.i}"
    case assignment: RegisterAssignment =>
      val register = getRegister(assignment.reg.r)
      s"$register=${getExpression(assignment.expression)}"
  }

  private def getRegister(r: Register): String = r match {
    case name: NameRegister => s"@${name.s}"
    case num: NumberRegister => s"@${num.i}"
    case special: SpecialRegister => special.s
    case static: StaticRegister => s"@${static.fileName}.${static.i}"
  }

  private def getValue(a:Value): String =  a.registerOrConstant match {
    case Left(reg) => getRegister(reg.r)
    case Right(const) => const.i.toString
  }

  private def getExpression(exp: Expression): String = exp match {
    case assign: AssignmentExpression => getValue(assign.a).toString
    case add: AddExpression => s"${getValue(add.a)}+${getValue(add.b)}"
    case sub: SubtractExpression => s"${getValue(sub.a)}-${getValue(sub.b)}"
    case neg: NegativeExpression => s"-${getValue(neg.a).toString}"
    case and: AndAssignmentExpression => s"${getValue(and.a)}&${getValue(and.b)}"
    case or: OrAssignmentExpression => s"${getValue(or.a)}|${getValue(or.b)}"
    case not: NotAssignmentExpression => s"!${getValue(not.a)}"
    case lt: LessThanExpression => if (getValue(lt.a) < getValue(lt.b)) "-1" else "0"
    case gt: GreaterThanExpression => if (getValue(gt.a) > getValue(gt.b)) "-1" else "0"
    case eq: EqualExpression =>   if (getValue(eq.a) == getValue(eq.b)) "-1" else "0"
  }
}