package kfulton.nand2tetris2.translator

import kfulton.nand2tetris2.parser._

class Translator(fileName: String) {
  val DRegister = RegisterExp(SpecialRegister("D"))
  val MRegister = RegisterExp(SpecialRegister("M"))
  val ARegister = RegisterExp(SpecialRegister("A"))
  val ARegisterValue = Value(Left(RegisterExp(SpecialRegister("A"))))
  val DRegisterValue = Value(Left(RegisterExp(SpecialRegister("D"))))
  val MRegisterValue = Value(Left(RegisterExp(SpecialRegister("M"))))
  val Const0RegisterValue = Value(Right(ConstantExp(0)))
  val Const1RegisterValue = Value(Right(ConstantExp(1)))
  val ConstNeg1RegisterValue = Value(Right(ConstantExp(-1)))
  val popVariable = "v"
  val stackPointer = "SP"

  def generateAssembly(vmCommand: Either[VMCommand, ParsingError], current: Int = 0): Either[List[AssemblyCommand], TranslationError] =
    vmCommand match {
      case Left(vmCommand) => translateToAssembly(vmCommand, current)
      case Right(parsingError) => Right(TranslationError(s"Could not translate parsing error: $parsingError"))
  }

  private def translateToAssembly(vmCommand: VMCommand, current: Int): Either[List[AssemblyCommand], TranslationError] = vmCommand match {
    case push: Push => Left(generatePushAssembly(push))
    case pop: Pop => Left(generatePopAssembly(pop)) //handle static
    case Add => Left(generateArithmeticAssembly(AddExpression(MRegisterValue, DRegisterValue)))
    case Subtract => Left(generateArithmeticAssembly(SubtractExpression(MRegisterValue, DRegisterValue)))
    case Negative => Left(generateNegativeAssembly)
    case Equal => Left(generateLogicAssembly(EqualExpression(MRegisterValue, DRegisterValue))) //-1 is true, false is 0
    case LessThan => Left(generateLogicAssembly(LessThanExpression(MRegisterValue, DRegisterValue)))
    case GreaterThan => Left(generateLogicAssembly(GreaterThanExpression(MRegisterValue, DRegisterValue)))
    case Not => Left(generateNotAssembly)
    case And => Left(generateLogicAssembly(AndAssignmentExpression(DRegisterValue, MRegisterValue)))
    case Or => Left(generateLogicAssembly(OrAssignmentExpression(DRegisterValue, MRegisterValue)))
    case goto: GoTo => Left(generateGoTo(goto.variable))
    case ifGoTo: IfGoTo => Left(generateIfGoTo(ifGoTo.variable))
    case label: Label => Left(generateLabel(label.variable))
    case FunctionReturn => Left(generateFunctionReturn(current))
    case _ => Right(TranslationError(s"Could not translate vmCommand: $vmCommand."))
  }

  private def generateFunctionReturn(current: Int) = ???

  private def generateGoTo(str: String): List[AssemblyCommand] = List(
    Branching(Variable(str), isGoto = true),
    UnconditionalJump(Const0RegisterValue)
  )

  private def generateLabel(str: String) = List(
    Branching(Variable(str), isLabel = true)
  )

  private def generateIfGoTo(str: String) = List(
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(str)),
    JumpNotEqual(DRegisterValue)
  )

  private def generatePushAssembly(push: Push): List[AssemblyCommand] = {
    if (push.segment.equals(Pointer)) {
      val segment: String = if (push.location.equals(0)) "THIS" else "THAT"
      List(
        RegisterExp(NameRegister(segment)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue))
      ) ++ addToStack
    } else if (push.segment.equals(Temp)) {
      List(
        RegisterExp(NumberRegister(5)),
        RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
        RegisterExp(NumberRegister(push.location)),
        RegisterAssignment(ARegister, AddExpression(DRegisterValue, ARegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue))
      ) ++ addToStack
    } else if (push.segment.equals(Constant)) { //confirm this is correct
      List(
        RegisterExp(NumberRegister(push.location)),
        RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue))
      ) ++ addToStack
    } else if (push.segment.equals(Static)) {
      List(
        RegisterExp(StaticRegister(fileName, push.location)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
      )
    } else {
      List(
        RegisterExp(NameRegister(push.segment.name)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(NumberRegister(push.location)),
        RegisterAssignment(ARegister, AddExpression(DRegisterValue, ARegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue))
      ) ++ addToStack
    }
  }

  private def addToStack = List(
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
  )

  private def generatePopAssembly(pop: Pop): List[AssemblyCommand] = {
    if (pop.segment.equals(Pointer)) {
      val segment: String = if (pop.location.equals(0)) "THIS" else "THAT"
      List(
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(NameRegister(segment)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
      )
    } else if (pop.segment.equals(Static)){
      List(
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(StaticRegister(fileName, pop.location)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
      )
    } else {
      val segmentLocation = if (pop.segment.equals(Temp)) {
        List(ConstantExp(5),
          RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)))
      } else {
        List(RegisterExp(NameRegister(pop.segment.name)),
          RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)))
      }
      segmentLocation ++ List(
        ConstantExp(pop.location),
        RegisterAssignment(ARegister, AddExpression(ARegisterValue, DRegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
        RegisterExp(NameRegister(popVariable)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(NameRegister(popVariable)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
      )
    }
  }

  private def generateArithmeticAssembly(expression: Expression): List[AssemblyCommand] =
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, expression),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

  private def generateNegativeAssembly =
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, NegativeExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

  private def generateNotAssembly =
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(ARegister, SubtractExpression(DRegisterValue, Const1RegisterValue)),
      RegisterAssignment(MRegister, NotAssignmentExpression(MRegisterValue))
    )

  private def generateLogicAssembly(expression: Expression) =
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, expression),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

  private def generateJumpAssembly(predicate: String, jumpCondition: AssemblyCommand, current: Int) = {
    val endGoTo = generateGoTo(s"END$current")
    val endLabel = generateLabel(s"END$current")
    val predicateGoTo = generateGoTo(s"$predicate$current")
    val predicateLabel = generateLabel(s"$predicate$current")
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, SubtractExpression(DRegisterValue, MRegisterValue))) ++
      predicateGoTo ++
      List(jumpCondition,
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(Const0RegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))) ++
      endGoTo ++ predicateLabel ++
      List(RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(ConstNeg1RegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))) ++
      endGoTo ++ endLabel ++ endGoTo
  }

  private def popFrom: List[AssemblyCommand] =
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue))
    )
}

