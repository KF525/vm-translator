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
  val translator = new Translator

  def runProgram(path: String, fileName: String) = {
    val files: Seq[File] = getFiles(new File(path))
    val orderedFiles = orderFiles(files)
    val completedLines: Seq[(String, String)] = generateCompletedLinesForAllFiles(orderedFiles)

    def generateAssemblyCode2(completedLines: Seq[(String, String)], assemblyCommands: Seq[String] = Seq(), current: Int = 0): Seq[String] =
      completedLines match {
        case Nil => assemblyCommands
        case h::t => generateAssemblyCode2(t, assemblyCommands ++ generateAssemblyCode(h, completedLines.size)) //maybe this doesn't change
    }

    writeAssemblyToFile(generateAssemblyCode2(completedLines), fileName)
  }


  private def generateCompletedLinesForAllFiles(orderedFiles: Seq[File]): Seq[(String, String)] = {
    orderedFiles.foldLeft(Seq[(String, String)]())((seq, file) => seq ++: generateCompletedLinesForFile(file))
  }

  private def generateCompletedLinesForFile(file: File): Seq[(String, String)] = {
    val lines = Source.fromFile(file).getLines()
    val initLines = if (file.getName.equals("successful/Sys.vm")) Seq(("init", "successful/Sys.vm"), ("call Sys.init 0", "successful/Sys.vm")) else Seq()
    val linesWithFileName = lines.foldLeft(Seq[(String, String)]())((seq, line) => seq ++: Seq((line, file.getName.split(".vm")(0))))
    initLines ++ linesWithFileName
  }


  private def orderFiles(files: Seq[File])= if (files.size > 1) {
    val sysFile = files.filter(file => file.getName.equals("Sys.vm"))
    val otherFiles = files.filterNot(file => file.getName.equals("Sys.vm"))
    sysFile ++ otherFiles
  } else files

  private def writeAssemblyToFile(assemblyCode: Seq[String], fileName: String): Path = {
    val content: Array[Byte] = assemblyCode.mkString("\n").getBytes
    Files.write(Paths.get(s"src/main/scala/$fileName.asm"), content)
  }

  private def generateAssemblyFile(commands: Seq[String], file: String): String = commands match {
    case Nil => file
    case h::t => generateAssemblyFile(t, file + h + "\n")
  }

  private def generateAssemblyCode(command: (String, String), current: Int = 0): List[String] = {
    val vmCommandsEither: Either[VMCommand, ParsingError] = parser.generateVMCommand(command._1)
    val assemblyCommandsEither: Either[List[AssemblyCommand], TranslationError] = translator.generateAssembly(vmCommandsEither, command._2, current)
    assemblyCommandsEither match {
     case Left(commands) => commands.map(printer.generateAssemblyCode(_))
     case Right(error) => List[String]()
   }
  }

  private def getFiles(file: File): List[File] = {
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
//  vmTranslator.runProgram("/Users/katefulton/Desktop/N2T/vm-translator/src/main/resources/successful/StackTest.vm", "StackTest")
  //vmTranslator.runProgram("/Users/katefulton/Desktop/N2T/vm-translator/src/main/resources/successful.FibonacciElement", "successful.FibonacciElement")
  vmTranslator.runProgram("/Users/katefulton/Desktop/N2T/vm-translator/src/main/resources/StaticsTest", "StaticsTest")
  //vmTranslator.runProgram("/Users/katefulton/Desktop/N2T/vm-translator/src/main/resources/successful/BasicLoop.vm", "BasicLoop")
//  vmTranslator.runProgram("/Users/katefulton/Desktop/N2T/vm-translator/src/main/resources/successful/FibonacciSeries.vm", "FibonacciSeries")
  // vmTranslator.runProgram("/Users/katefulton/Desktop/N2T/vm-translator/src/main/resources/successful/SimpleFunction.vm", "SimpleFunction")
//  vmTranslator.runProgram("/Users/katefulton/Desktop/N2T/vm-translator/src/main/resources/successful/StackTest.vm", "StackTest")
}