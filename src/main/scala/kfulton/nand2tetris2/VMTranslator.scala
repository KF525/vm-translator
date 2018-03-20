package kfulton.nand2tetris2


import java.io.File
import java.nio.file.{Files, Path, Paths}

import kfulton.nand2tetris2.parser.{Parser, ParsingError, VMCommand}
import kfulton.nand2tetris2.printer.Printer
import kfulton.nand2tetris2.translator.{AssemblyCommand, TranslationError, Translator}

import scala.io.Source

class VMTranslator {
  val parser = new Parser
  val printer = new Printer

  def runProgram(path: String, fileName: String) = {
    val files: Seq[File] = getFiles(new File(path))

    val linesWithFileName: Seq[(List[String], String)] = for {
      file <- files
      lines = Source.fromFile(file).getLines()
    } yield (lines.toList, file.getName)

    def generateAssemblyCode2(lines: Seq[String], fileName: String, assemblyCommands: Seq[String] = Seq()): Seq[String] = lines match {
      case Nil => assemblyCommands
      case h::t => generateAssemblyCode2(t, fileName, assemblyCommands ++ generateAssemblyCode(h, fileName, lines.size))
    }
    val stringCommands = for {
      (lines, fileName) <- linesWithFileName
      assemblyCommands = generateAssemblyCode2(lines, fileName)
    } yield generateAssemblyFile(assemblyCommands, "")

    writeAssemblyToFile(stringCommands, fileName)
  }

  def writeAssemblyToFile(assemblyCode: Seq[String], fileName: String): Path = {
    val content: Array[Byte] = assemblyCode.mkString("\n").getBytes
    Files.write(Paths.get(s"src/main/scala/$fileName.asm"), content)
  }

  def generateAssemblyFile(commands: Seq[String], file: String): String = commands match {
    case Nil => file
    case h::t => generateAssemblyFile(t, file + h + "\n")
  }

  def generateAssemblyCode(command: String, fileName: String, current: Int = 0): List[String] = {
    val vmCommandsEither: Either[VMCommand, ParsingError] = parser.generateVMCommand(command)
    val assemblyCommandsEither: Either[List[AssemblyCommand], TranslationError] = new Translator(fileName).generateAssembly(vmCommandsEither, current)
    assemblyCommandsEither match {
     case Left(commands) => commands.map(printer.generateAssemblyCode(_))
     case Right(error) => List[String]()
   }
  }

  def getFiles(file: File): List[File] = {
    if (file.exists() && file.isDirectory) {
     file.listFiles.filter(_.isFile).toList
    } else if (file.exists()) {
      List(file)
    } else {
      List.empty
    }
  }
}

object VMTranslator extends App {
  val vmTranslator = new VMTranslator
  vmTranslator.runProgram("/Users/katefulton/Desktop/N2T/vm-translator/src/main/resources/FibonacciSeries.vm", "FibonacciSeries")
}

//exceptions should be reserved for programmer errors

/*
parse: Str => Either[VMCommand, ParsingError]
translate: Str => Either[AssemblyCommand, TranslationError]
printAss: Assembly => String
compiler: String => String
 */