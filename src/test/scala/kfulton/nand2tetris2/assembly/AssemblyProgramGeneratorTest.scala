package kfulton.nand2tetris2.assembly

import org.scalatest.{FlatSpec, Matchers}

class AssemblyProgramGeneratorTest extends FlatSpec with Matchers {
  val assemblyProgramGenerator = new AssemblyProgramGenerator
  "generateAssemblyCode" should "format jumps correctly" in {
    val registerExp = Left(RegisterExp(SpecialRegister("A")))
    val constantExp = Right(ConstantExp(1))

    assemblyProgramGenerator.generateAssemblyCode(Jump(Value(constantExp))) shouldBe "1; JMP"
    assemblyProgramGenerator.generateAssemblyCode(Jump(Value(registerExp))) shouldBe "A; JMP"
    assemblyProgramGenerator.generateAssemblyCode(JumpNotEqual(Value(registerExp))) shouldBe "A; JNE"
    assemblyProgramGenerator.generateAssemblyCode(JumpEqual(Value(registerExp))) shouldBe "A; JEQ"
    assemblyProgramGenerator.generateAssemblyCode(JumpGreaterThan(Value(registerExp))) shouldBe "A; JGT"
    assemblyProgramGenerator.generateAssemblyCode(JumpGreaterThanOrEqual(Value(registerExp))) shouldBe "A; JGE"
    assemblyProgramGenerator.generateAssemblyCode(JumpLessThan(Value(registerExp))) shouldBe "A; JLT"
    assemblyProgramGenerator.generateAssemblyCode(JumpLessThanOrEqual(Value(registerExp))) shouldBe "A; JLE"
  }

  it should "format assignments correctly" in {
    val constantExp = ConstantExp(1)
    val addExpression = AddExpression(Value(Right(constantExp)), Value(Right(constantExp)))
    assemblyProgramGenerator.generateAssemblyCode(RegisterAssignment(RegisterExp(NameRegister("SP")), addExpression)) shouldBe "@SP = 1 + 1"
  }

  "getRegister" should "return the correct register" in {
    assemblyProgramGenerator.getRegister(NameRegister("a")) shouldBe "@a"
    assemblyProgramGenerator.getRegister(NumberRegister(2)) shouldBe "@2"
    assemblyProgramGenerator.getRegister(SpecialRegister("A")) shouldBe "A"
  }

  "getExpression" should "return the correct expression" in {
    val constantExp = ConstantExp(1)
    val assignmentExpression = AssignmentExpression(Value(Right(constantExp)))
    val addExpression = AddExpression(Value(Right(constantExp)), Value(Right(constantExp)))
    val subtractExpression = SubtractExpression(Value(Right(constantExp)), Value(Right(constantExp)))

    assemblyProgramGenerator.getExpression(assignmentExpression) shouldBe "1"
    assemblyProgramGenerator.getExpression(addExpression) shouldBe "1 + 1"
    assemblyProgramGenerator.getExpression(subtractExpression) shouldBe "1 - 1"
  }
}
