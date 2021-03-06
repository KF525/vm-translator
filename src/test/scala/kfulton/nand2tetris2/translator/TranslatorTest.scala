package kfulton.nand2tetris2.translator

import kfulton.nand2tetris2.parser._
import org.scalatest.{FlatSpec, Matchers}

//smaller scope, shorter the name
class TranslatorTest extends FlatSpec with Matchers {
  val translator = new Translator

  val DRegister = RegisterExp(SpecialRegister("D"))
  val MRegister = RegisterExp(SpecialRegister("M"))
  val ARegister = RegisterExp(SpecialRegister("A"))
  val ARegisterValue = Value(Left(RegisterExp(SpecialRegister("A"))))
  val DRegisterValue = Value(Left(RegisterExp(SpecialRegister("D"))))
  val MRegisterValue = Value(Left(RegisterExp(SpecialRegister("M"))))
  val Const0RegisterValue = Value(Right(ConstantExp(0)))
  val Const1RegisterValue = Value(Right(ConstantExp(1)))
  val ConstNeg1RegisterValue = Value(Right(ConstantExp(-1)))

  "translateToAssembly" should "handle push correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("LCL")),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NumberRegister(4)),
      RegisterAssignment(ARegister, AddExpression(DRegisterValue, ARegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )
    translator.generateAssembly(Left(Push(Local, 4)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle temp push correctly" in {
    val assemblyCommands = List(
      RegisterExp(NumberRegister(5)),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NumberRegister(6)),
      RegisterAssignment(ARegister, AddExpression(DRegisterValue, ARegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

    translator.generateAssembly(Left(Push(Temp, 6)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle pointer push referencing this correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("THIS")),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

    translator.generateAssembly(Left(Push(Pointer, 0)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle pointer push referencing that correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("THAT")),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

    translator.generateAssembly(Left(Push(Pointer, 1)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle static push correctly" in {
    val assemblyCommands =   List(
      RegisterExp(StaticRegister("fileName", 3)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

    translator.generateAssembly(Left(Push(Static, 3)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle constant push correctly" in {
    val assemblyCommand = List(
      RegisterExp(NumberRegister(4)),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

    translator.generateAssembly(Left(Push(Constant, 4)), "fileName") shouldBe Left(assemblyCommand)
  }

  it should "handle pop correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("LCL")),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      ConstantExp(4),
      RegisterAssignment(ARegister, AddExpression(ARegisterValue, DRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister("v")),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("v")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
    )

    translator.generateAssembly(Left(Pop(Local, 4)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle temp pop correctly" in {
    val assemblyCommands = List(
      ConstantExp(5),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      ConstantExp(4),
      RegisterAssignment(ARegister, AddExpression(ARegisterValue, DRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister("v")),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("v")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
    )

    translator.generateAssembly(Left(Pop(Temp, 4)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle 'this' pointer pop correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("THIS")),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
    )

    translator.generateAssembly(Left(Pop(Pointer, 0)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle 'that' pointer pop correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("THAT")),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
    )

    translator.generateAssembly(Left(Pop(Pointer, 1)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle static pop correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(StaticRegister("fileName", 5)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
    )

    translator.generateAssembly(Left(Pop(Static, 5)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle add correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Value(Right(ConstantExp(1)))))
    )

    translator.generateAssembly(Left(Add), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle subtract correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Value(Right(ConstantExp(1)))))
    )

    translator.generateAssembly(Left(Subtract), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle turning a value negative correctly" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, NegativeExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Value(Right(ConstantExp(1)))))
    )

    translator.generateAssembly(Left(Negative), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle and" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AndAssignmentExpression(DRegisterValue, MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Value(Right(ConstantExp(1)))))
    )

    translator.generateAssembly(Left(And), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle or" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, OrAssignmentExpression(DRegisterValue, MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Value(Right(ConstantExp(1)))))
    )

    translator.generateAssembly(Left(Or), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle not" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(ARegister, SubtractExpression(DRegisterValue, Const1RegisterValue)),
      RegisterAssignment(MRegister, NotAssignmentExpression(MRegisterValue))
    )

    translator.generateAssembly(Left(Not), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle goto" in {
    val assemblyCommands = List(
      GoToA(NameRegister("end3")),
      UnconditionalJump(Value(Right(ConstantExp(0))))
    )

    translator.generateAssembly(Left(GoTo("end3")), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle label" in {
    val assemblyCommands = List(
      LabelA("end3")
    )

    translator.generateAssembly(Left(Label("end3")), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle if goto" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("end2")),
      JumpNotEqual(DRegisterValue)
    )

    translator.generateAssembly(Left(IfGoTo("end2")), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle function" in {
    val assemblyCommands = List(
      LabelA("FunctionName"),
      RegisterExp(NumberRegister(0)),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue)),
      RegisterExp(NumberRegister(0)),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

    translator.generateAssembly(Left(Function("FunctionName", 2)), "fileName") shouldBe Left(assemblyCommands)
  }

  it should "handle function call" in {
    val assemblyCommands = List(
      RegisterExp(NameRegister("RET3")),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister,AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister,AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister,AddExpression(MRegisterValue,Value(Right(ConstantExp(1))))),
      RegisterExp(NameRegister("LCL")),
      RegisterAssignment(DRegister,AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister,AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister,AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister,AddExpression(MRegisterValue,Value(Right(ConstantExp(1))))),
      RegisterExp(NameRegister("ARG")),
      RegisterAssignment(DRegister,AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister,AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister,AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister,AddExpression(MRegisterValue,Value(Right(ConstantExp(1))))),
      RegisterExp(NameRegister("THIS")),
      RegisterAssignment(DRegister,AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister,AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister,AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister,AddExpression(MRegisterValue,Value(Right(ConstantExp(1))))),
      RegisterExp(NameRegister("THAT")),
      RegisterAssignment(DRegister,AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister,AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister,AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister,AddExpression(MRegisterValue,Value(Right(ConstantExp(1))))),
      ConstantExp(5),
      RegisterAssignment(DRegister,AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(DRegister,SubtractExpression(MRegisterValue,DRegisterValue)),
      ConstantExp(2),
      RegisterAssignment(DRegister,SubtractExpression(DRegisterValue,ARegisterValue)),
      RegisterExp(NameRegister("ARG")),
      RegisterAssignment(MRegister,AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(DRegister,AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("LCL")),
      RegisterAssignment(MRegister,AssignmentExpression(DRegisterValue)),
      GoToA(NameRegister("FunctionName")),
      UnconditionalJump(Value(Right(ConstantExp(0)))),
      LabelA("RET3")
    )

    translator.generateAssembly(Left(FunctionCall("FunctionName", 2)),"fileName", 3) shouldBe Left(assemblyCommands)
  }

  it should "handle function return" in {

  }
}