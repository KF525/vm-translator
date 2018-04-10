package kfulton.nand2tetris2.parser

import scala.util.matching.Regex.Match
import scala.util.{Success, Try}

class Parser {
  val VMCapturePattern =  "^\\s*(\\w+\\S*\\w*)(?:\\s+(\\w+\\S*\\w*)\\s+(\\d+))?(?:\\s+(\\S+))?.*$".r

  def generateVMCommand(command: String): Either[VMCommand, ParsingError] = {
    val maybeRegexMatch = VMCapturePattern.findFirstMatchIn(command)
    maybeRegexMatch match {
      case Some(regexMatch) => findVMCommand(regexMatch)
      case None => Right(ParsingError(s"Could not match command: $command."))
    }
  }

  private def findVMCommand(result: Match) =
    result.group(1).toLowerCase() match {
      case "push" => createMemoryCommand(result, Push)
      case "pop" => createMemoryCommand(result, Pop)
      case "goto" => createBranchingCommand(result)
      case "label" => createBranchingCommand(result)
      case "if-goto" => createBranchingCommand(result)
      case "function" => createFunctionCommand(result)
      case "call" =>   createFunctionCommand(result)
      case "return" => Left(FunctionReturn)
      case s: String => VMCommand.fromString(s) match {
        case Some(v) => Left(v)
        case None => Right(ParsingError(s"Could not match: $result."))
      }
      case _ => Right(ParsingError(s"Could not match: $result."))
    }

  private def createBranchingCommand(result: Match) = {
    val branchType = result.group(1)
    val name = result.group(4)

    branchType match {
      case "goto" => Left(GoTo(name))
      case "label" => Left(Label(name))
      case "if-goto" => Left(IfGoTo(name))
      case _ => Right(ParsingError(s"Could not parse: $result"))
    }
  }

  private def createFunctionCommand(result: Match) = result.group(1) match {
    case "function" =>
      val functionName = result.group(2)
      val locationTry = Try(result.group(3).toInt)
      locationTry match {
        case Success(i) => Left(Function(functionName, i))
        case _ => Right(ParsingError(s"Invalid number of arguments ${result.group(3)} for $result."))
      }
    case "call" =>
      val functionName = result.group(2)
      val locationTry = Try(result.group(3).toInt)
      locationTry match {
        case Success(i) => Left(FunctionCall(functionName, i))
        case _ => Right(ParsingError(s"Invalid number of arguments ${result.group(3)} for $result."))
      }
  }

  private def createMemoryCommand(result: Match, createFunction: (VMSegment, Int) => VMCommand) =
    VMSegment.fromString(result.group(2)) match {
      case Some(segment) =>
        val locationTry = Try(result.group(3).toInt)
        locationTry match {
          case Success(i) => Left(createFunction(segment, i))
          case _ => Right(ParsingError(s"Invalid location ${result.group(3)} for $result."))
        }
      case None => Right(ParsingError(s"Could not find segment: ${result.group(2)} for $result."))
    }
}