package kfulton.nand2tetris2.translator

case class Value(registerOrConstant: Either[RegisterExp, ConstantExp])

sealed abstract class Register extends AssemblyCommand
case class NameRegister(s: String) extends Register
case class NumberRegister(i: Int) extends Register
case class SpecialRegister(s: String) extends Register
case class StaticRegister(fileName: String, i: Int) extends Register

case class ConstantExp(i: Int) extends AssemblyCommand
case class RegisterExp(r: Register) extends AssemblyCommand

trait Expression extends AssemblyCommand
case class AssignmentExpression(a: Value) extends Expression
case class AddExpression(a: Value, b: Value) extends Expression
case class SubtractExpression(a: Value, b: Value) extends Expression
case class NegativeExpression(a: Value) extends Expression
case class AndAssignmentExpression(a: Value, b:Value) extends Expression
case class OrAssignmentExpression(a:Value, b:Value) extends Expression
case class NotAssignmentExpression(a:Value) extends Expression
case class LessThanExpression(a: Value, b: Value) extends Expression
case class GreaterThanExpression(a: Value, b: Value) extends Expression
case class EqualExpression(a: Value, b: Value) extends Expression

trait AssemblyCommand
case class RegisterAssignment(reg: RegisterExp, expression: Expression) extends AssemblyCommand
case class JumpNotEqual(a: Value) extends AssemblyCommand
case class JumpEqual(a: Value) extends AssemblyCommand
case class JumpGreaterThan(a: Value) extends AssemblyCommand
case class JumpGreaterThanOrEqual(a: Value) extends AssemblyCommand
case class JumpLessThan(a: Value) extends AssemblyCommand
case class JumpLessThanOrEqual(a: Value) extends AssemblyCommand
case class UnconditionalJump(a: Value) extends AssemblyCommand
case class LabelA(name: String) extends AssemblyCommand
case class GoToA(name: String) extends AssemblyCommand