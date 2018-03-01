package kfulton.nand2tetris2.vmcommand
import scala.util.matching.Regex.Match
import scala.util.{Success, Try}

class VMCommandMatcher {
  val VMCapturePattern =  "^\\s*(\\w+)(?:\\s+(\\w+)\\s+(\\d+))?\\s*$".r

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
      case s: String => VMCommand.fromString(s) match {
        case Some(v) => Left(v)
        case None => Right(ParsingError(s"Could not match: $result."))
      }
      case _ => Right(ParsingError(s"Could not match: $result."))
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