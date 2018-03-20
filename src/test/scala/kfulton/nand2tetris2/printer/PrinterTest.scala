package kfulton.nand2tetris2.printer

import kfulton.nand2tetris2.translator._
import org.scalatest.{FlatSpec, Matchers}

class PrinterTest extends FlatSpec with Matchers {
  val assemblyProgramGenerator = new Printer
  "generateAssemblyCode" should "format jumps correctly" in {
    val registerExp = Left(RegisterExp(SpecialRegister("A")))
    val constantExp = Right(ConstantExp(1))

    assemblyProgramGenerator.generateAssemblyCode(UnconditionalJump(Value(constantExp))) shouldBe "1; JMP"
    assemblyProgramGenerator.generateAssemblyCode(UnconditionalJump(Value(registerExp))) shouldBe "A; JMP"
    assemblyProgramGenerator.generateAssemblyCode(JumpNotEqual(Value(registerExp))) shouldBe "A; JNE"
    assemblyProgramGenerator.generateAssemblyCode(JumpEqual(Value(registerExp))) shouldBe "A; JEQ"
    assemblyProgramGenerator.generateAssemblyCode(JumpGreaterThan(Value(registerExp))) shouldBe "A; JGT"
    assemblyProgramGenerator.generateAssemblyCode(JumpGreaterThanOrEqual(Value(registerExp))) shouldBe "A; JGE"
    assemblyProgramGenerator.generateAssemblyCode(JumpLessThan(Value(registerExp))) shouldBe "A; JLT"
    assemblyProgramGenerator.generateAssemblyCode(JumpLessThanOrEqual(Value(registerExp))) shouldBe "A; JLE"
  }

  it should "format branching correctly" in {
    val goto = Branching(Variable("variable2"), isGoto = true)
    val label = Branching(Variable("variable2"), isLabel = true)

    assemblyProgramGenerator.generateAssemblyCode(goto) shouldBe "@variable2"
    assemblyProgramGenerator.generateAssemblyCode(label) shouldBe "(variable2)"
  }

  it should "format constants correctly" in {
    val constantExp = ConstantExp(1)
    assemblyProgramGenerator.generateAssemblyCode(constantExp) shouldBe "@1"
  }

  "getRegister" should "return the correct register" in {
    assemblyProgramGenerator.generateAssemblyCode(RegisterExp(NameRegister("a"))) shouldBe "@a"
    assemblyProgramGenerator.generateAssemblyCode(RegisterExp(NumberRegister(2))) shouldBe "@2"
    assemblyProgramGenerator.generateAssemblyCode(RegisterExp(SpecialRegister("A"))) shouldBe "A"
    assemblyProgramGenerator.generateAssemblyCode(RegisterExp(StaticRegister("fileName", 3))) shouldBe "@fileName.3"
  }

  "getExpression" should "return the correct expression" in {
    val regExpression = RegisterExp(SpecialRegister("M"))
    val constantValue1 = Value(Right(ConstantExp(1)))
    val constantValue2 = Value(Right(ConstantExp(2)))
    val assignmentExpression = RegisterAssignment(regExpression, AssignmentExpression(constantValue1))
    val addExpression = RegisterAssignment(regExpression, AddExpression(constantValue1, constantValue1))
    val subtractExpression = RegisterAssignment(regExpression, SubtractExpression(constantValue1, constantValue1))
    val negativeExpression = RegisterAssignment(regExpression, NegativeExpression(constantValue1))
    val andExpression = RegisterAssignment(regExpression, AndAssignmentExpression(constantValue1, constantValue1))
    val orExpression = RegisterAssignment(regExpression, OrAssignmentExpression(constantValue1, constantValue1))
    val notExpression = RegisterAssignment(regExpression, NotAssignmentExpression(constantValue1))
    val ltExpression1 = RegisterAssignment(regExpression, LessThanExpression(constantValue1, constantValue1))
    val ltExpression2 = RegisterAssignment(regExpression, LessThanExpression(constantValue1, constantValue2))
    val gtExpression1 = RegisterAssignment(regExpression, GreaterThanExpression(constantValue1, constantValue1))
    val gtExpression2 = RegisterAssignment(regExpression, GreaterThanExpression(constantValue2, constantValue1))
    val eqExpression1 = RegisterAssignment(regExpression, EqualExpression(constantValue1, constantValue1))
    val eqExpression2 = RegisterAssignment(regExpression, EqualExpression(constantValue1, constantValue2))

    assemblyProgramGenerator.generateAssemblyCode(assignmentExpression) shouldBe "M=1"
    assemblyProgramGenerator.generateAssemblyCode(addExpression) shouldBe "M=1+1"
    assemblyProgramGenerator.generateAssemblyCode(subtractExpression) shouldBe "M=1-1"
    assemblyProgramGenerator.generateAssemblyCode(negativeExpression) shouldBe "M=-1"
    assemblyProgramGenerator.generateAssemblyCode(andExpression) shouldBe "M=1&1"
    assemblyProgramGenerator.generateAssemblyCode(orExpression) shouldBe "M=1|1"
    assemblyProgramGenerator.generateAssemblyCode(notExpression) shouldBe "M=!1"
    assemblyProgramGenerator.generateAssemblyCode(ltExpression1) shouldBe "M=0"
    assemblyProgramGenerator.generateAssemblyCode(ltExpression2) shouldBe "M=-1"
    assemblyProgramGenerator.generateAssemblyCode(gtExpression1) shouldBe "M=0"
    assemblyProgramGenerator.generateAssemblyCode(gtExpression2) shouldBe "M=-1"
    assemblyProgramGenerator.generateAssemblyCode(eqExpression1) shouldBe "M=-1"
    assemblyProgramGenerator.generateAssemblyCode(eqExpression2) shouldBe "M=0"
  }
}
