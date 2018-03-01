package kfulton.nand2tetris2.vmcommand

import org.scalatest.{FlatSpec, Matchers}

class VMCommandMatcherTest extends FlatSpec with Matchers {
  val vMCommandMatcher = new VMCommandMatcher

  "generateVMCommand" should "return the correct VM Command" in {
    vMCommandMatcher.generateVMCommand("add") shouldBe Left(Add)
  }

  it should "ignore white space" in {
    vMCommandMatcher.generateVMCommand("   add") shouldBe Left(Add)
    vMCommandMatcher.generateVMCommand("add   ") shouldBe Left(Add)
  }

  it should "handle non-matches" in {
    vMCommandMatcher.generateVMCommand("bad") shouldBe Right(ParsingError("Could not match: bad."))
  }

  it should "handle push/pop inputs correctly" in {
    vMCommandMatcher.generateVMCommand("pop static 4") shouldBe Left(Pop(Static, 4))
    vMCommandMatcher.generateVMCommand("push static 4") shouldBe Left(Push(Static, 4))
  }

  it should "handle bad push/pop input" in {
    vMCommandMatcher.generateVMCommand("pop bad 4") shouldBe
      Right(ParsingError("Could not find segment: bad for pop bad 4."))
    vMCommandMatcher.generateVMCommand("pop static bad") shouldBe
      Right(ParsingError("Could not match command: pop static bad."))
    vMCommandMatcher.generateVMCommand("pop static 4.3") shouldBe
      Right(ParsingError("Could not match command: pop static 4.3."))
  }
}
