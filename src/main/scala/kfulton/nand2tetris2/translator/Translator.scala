package kfulton.nand2tetris2.translator

import kfulton.nand2tetris2.parser._

class Translator {
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

  def generateAssembly(vmCommand: Either[VMCommand, ParsingError], fileName: String, current: Int = 0): Either[List[AssemblyCommand], TranslationError] =
    vmCommand match {
      case Left(vmCommand) => translateToAssembly(vmCommand, fileName, current)
      case Right(parsingError) => Right(TranslationError(s"Could not translate parsing error: $parsingError"))
  }

  private def translateToAssembly(vmCommand: VMCommand, fileName: String, current: Int): Either[List[AssemblyCommand], TranslationError] = vmCommand match {
    case InitSP => Left(generateInitSP)
    case push: Push => Left(generatePushAssembly(push, fileName))
    case pop: Pop => Left(generatePopAssembly(pop, fileName))
    case Add => Left(generateArithmeticAssembly(AddExpression(MRegisterValue, DRegisterValue)))
    case Subtract => Left(generateArithmeticAssembly(SubtractExpression(MRegisterValue, DRegisterValue)))
    case Negative => Left(generateNegativeAssembly)
    case Equal => Left(generateJumpAssembly("EQ", JumpEqual(DRegisterValue), current))
    case LessThan => Left(generateJumpAssembly("LT", JumpLessThan(DRegisterValue), current))
    case GreaterThan => Left(generateJumpAssembly("GT", JumpGreaterThan(DRegisterValue), current))
    case Not => Left(generateNotAssembly)
    case And => Left(generateLogicAssembly(AndAssignmentExpression(DRegisterValue, MRegisterValue)))
    case Or => Left(generateLogicAssembly(OrAssignmentExpression(DRegisterValue, MRegisterValue)))
    case goto: GoTo => Left(generateGoTo(goto.variable))
    case ifGoTo: IfGoTo => Left(generateIfGoTo(ifGoTo.variable))
    case label: Label => Left(generateLabel(label.variable))
    case FunctionReturn => Left(generateFunctionReturn(current))
    case function: Function => Left(generateFunction(function.functionName, function.localVars, fileName))
    case functionCall: FunctionCall => Left(generateFunctionCall(functionCall.functionName, functionCall.nArgs, current))
    case _ => Right(TranslationError(s"Could not translate vmCommand: $vmCommand."))
  }

  private def generateInitSP = List(
    ConstantExp(256),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
  )

  private def generateFunction(functionName: String, localVars: Int, fileName: String, assemblyCommands: List[AssemblyCommand] = List()): List[AssemblyCommand] =
    localVars match {
      case 0 => List(LabelA(functionName)) ++ assemblyCommands
      case _ => generateFunction(functionName, localVars - 1, fileName, assemblyCommands ++ generatePushAssembly(Push(Constant, 0), fileName))
    }

  private def saveSegmentPlacetoStack(segment: String, value: Value) = List(
    RegisterExp(NameRegister(segment)),
    RegisterAssignment(DRegister, AssignmentExpression(value)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
  )

  private def generateFunctionCall(functionName: String, args: Int, current: Int) = {
    List(
      RegisterExp(NameRegister(s"RET$current")),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister("SP")),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    ) ++
      saveSegmentPlacetoStack("LCL", MRegisterValue) ++
      saveSegmentPlacetoStack("ARG", MRegisterValue) ++
      saveSegmentPlacetoStack("THIS", MRegisterValue) ++
      saveSegmentPlacetoStack("THAT", MRegisterValue) ++
    List(
      ConstantExp(5),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(DRegister, SubtractExpression(MRegisterValue, DRegisterValue)),
      ConstantExp(args),
      RegisterAssignment(DRegister, SubtractExpression(DRegisterValue, ARegisterValue)),
      RegisterExp(NameRegister("ARG")),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister("LCL")),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
    ) ++
      generateGoTo(functionName) ++
      generateLabel(s"RET$current")

  }

  private def generateFunctionReturn(current: Int) = List(
    RegisterExp(NameRegister("LCL")),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    ConstantExp(5),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(s"RET$current")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister("ARG")),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    RegisterExp(NameRegister("ARG")),
    RegisterAssignment(DRegister, AddExpression(MRegisterValue, Const1RegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    ConstantExp(1),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister("THAT")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    ConstantExp(2),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister("THIS")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    ConstantExp(3),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister("ARG")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    ConstantExp(4),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister("LCL")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    GoToA(s"RET$current"),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    UnconditionalJump(Const0RegisterValue)
  )

  private def generateGoTo(str: String): List[AssemblyCommand] = List(
    GoToA(str),//TODO: Can variable be register name?
    UnconditionalJump(Const0RegisterValue)
  )

  private def generateLabel(str: String) = List(
    LabelA(str)
  )

  private def generateIfGoTo(str: String) = List(
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(str)),
    JumpNotEqual(DRegisterValue)
  )

  private def generatePushAssembly(push: Push, fileName: String): List[AssemblyCommand] = {
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

  private def generatePopAssembly(pop: Pop, fileName: String): List[AssemblyCommand] = {
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

  private def generateLogicAssembly(expression: Expression) = //need a label here
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
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, SubtractExpression(MRegisterValue, DRegisterValue)),
      GoToA(s"$predicate$current"),
      jumpCondition,
      RegisterAssignment(DRegister, AssignmentExpression(Const0RegisterValue)),
      GoToA(s"END$current"),
      JumpEqual(Const0RegisterValue),
      LabelA(s"$predicate$current"),
      RegisterAssignment(DRegister, AssignmentExpression(ConstNeg1RegisterValue)),
      LabelA(s"END$current"),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )
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

