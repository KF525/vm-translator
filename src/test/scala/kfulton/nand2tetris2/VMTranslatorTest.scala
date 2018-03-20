package kfulton.nand2tetris2

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class VMTranslatorTest extends FlatSpec with Matchers {
  val vmTranslator = new VMTranslator

  "getFiles" should "return a list of all files within a directory" in {
    val path = getClass.getResource("/vmFolder")
    val folder = new File(path.getPath)
    val filePath1 = "/Users/katefulton/Desktop/N2T/vm-translator/target/classes/vmFolder/file.vm"
    val filePath2 = "/Users/katefulton/Desktop/N2T/vm-translator/target/classes/vmFolder/file2.vm"

    vmTranslator.getFiles(folder) shouldBe List(new File(filePath1), new File(filePath2))
  }
  it should "return a single file if there is no directory" in {
    val path = getClass.getResource("/filewithoutfolder.vm")
    val file = new File(path.getPath)
    val filePath = "/Users/katefulton/Desktop/N2T/vm-translator/target/classes/filewithoutfolder.vm"

    vmTranslator.getFiles(file) shouldBe List(new File(filePath))
  }
}
