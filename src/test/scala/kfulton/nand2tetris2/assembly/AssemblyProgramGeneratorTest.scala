package kfulton.nand2tetris2.assembly

import org.scalatest.{FlatSpec, Matchers}

class AssemblyProgramGeneratorTest extends FlatSpec with Matchers {
  val assemblyProgramGenerator = new AssemblyProgramGenerator
  "generateAssemblyCode" should "format jumps correctly" in {
    val registerExp = Left(RegisterExp(ARegister))
    val constantExp = Right(ConstantExp(1))

    assemblyProgramGenerator.generateAssemblyCode(Jump(constantExp)) shouldBe "1; JMP"
    assemblyProgramGenerator.generateAssemblyCode(Jump(registerExp)) shouldBe "A; JMP"
    assemblyProgramGenerator.generateAssemblyCode(JumpNotEqual(registerExp)) shouldBe "A; JNE"
    assemblyProgramGenerator.generateAssemblyCode(JumpEqual(registerExp)) shouldBe "A; JEQ"
    assemblyProgramGenerator.generateAssemblyCode(JumpGreaterThan(registerExp)) shouldBe "A; JGT"
    assemblyProgramGenerator.generateAssemblyCode(JumpGreaterThanOrEqual(registerExp)) shouldBe "A; JGE"
    assemblyProgramGenerator.generateAssemblyCode(JumpLessThan(registerExp)) shouldBe "A; JLT"
    assemblyProgramGenerator.generateAssemblyCode(JumpLessThanOrEqual(registerExp)) shouldBe "A; JLE"
  }

  it should "format assignments correctly" in {
    val constantExp = ConstantExp(1)
    val addExpression = AddExpression(Right(constantExp), Right(constantExp))
    assemblyProgramGenerator.generateAssemblyCode(ValueAssignment(ConstantExp(1), addExpression)) shouldBe "@1 = 1 + 1"
    assemblyProgramGenerator.generateAssemblyCode(RegisterAssignment(RegisterExp(ARegister), addExpression)) shouldBe "A = 1 + 1"
  }

  //TODO: How should it handle errors?

  "getRegister" should "return the correct register" in {
    assemblyProgramGenerator.getRegister(VariableRegister("a")) shouldBe "@a"
    assemblyProgramGenerator.getRegister(NumberRegister(2)) shouldBe "@2"
    assemblyProgramGenerator.getRegister(StackPointer) shouldBe "@SP"
    assemblyProgramGenerator.getRegister(ArgumentPointer) shouldBe "@ARG"
    assemblyProgramGenerator.getRegister(LocalPointer) shouldBe "@LCL"
    assemblyProgramGenerator.getRegister(ThisPointer) shouldBe "@THIS"
    assemblyProgramGenerator.getRegister(ThatPointer) shouldBe "@THAT"
    assemblyProgramGenerator.getRegister(MRegister) shouldBe "M"
    assemblyProgramGenerator.getRegister(DRegister) shouldBe "D"
    assemblyProgramGenerator.getRegister(ARegister) shouldBe "A"
  }

  //TODO: How should it handle errors?

  "getExpression" should "return the correct expression" in {
    val constantExp = ConstantExp(1)
    val equalExpression = EqualExpression(Right(constantExp))
    val addExpression = AddExpression(Right(constantExp), Right(constantExp))
    val subtractExpression = SubtractExpression(Right(constantExp), Right(constantExp))

    assemblyProgramGenerator.getExpression(equalExpression) shouldBe "1"
    assemblyProgramGenerator.getExpression(addExpression) shouldBe "1 + 1"
    assemblyProgramGenerator.getExpression(subtractExpression) shouldBe "1 - 1"
  }

  //TODO: How should it handle errors?
}
