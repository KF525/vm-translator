package kfulton.nand2tetris2.parser

import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {
  val vMCommandMatcher = new Parser

  "generateVMCommand" should "return the correct VM Command" in {
    vMCommandMatcher.generateVMCommand("add") shouldBe Left(Add)
  }

  it should "ignore white space" in {
    vMCommandMatcher.generateVMCommand("   add") shouldBe Left(Add)
    vMCommandMatcher.generateVMCommand("add   ") shouldBe Left(Add)
  }

  it should "handle non-matches" in {
    vMCommandMatcher.generateVMCommand("bad") shouldBe Right(ParsingError("Could not match: bad."))
    //    vMCommandMatcher.generateVMCommand("pop static 4.3") shouldBe
    //      Right(ParsingError("Could not match command: null for pop static 4.3.")) TODO: Do I want this to fail?

  }

  it should "handle valid inputs correctly" in {
    vMCommandMatcher.generateVMCommand("pop static 4") shouldBe Left(Pop(Static, 4))
    vMCommandMatcher.generateVMCommand("push static 4") shouldBe Left(Push(Static, 4))
    vMCommandMatcher.generateVMCommand("goto START_LOOP") shouldBe Left(GoTo("START_LOOP"))
    vMCommandMatcher.generateVMCommand("if-goto START_LOOP") shouldBe Left(IfGoTo("START_LOOP"))
    vMCommandMatcher.generateVMCommand("label START_LOOP") shouldBe Left(Label("START_LOOP"))
    vMCommandMatcher.generateVMCommand("add") shouldBe Left(Add)
    vMCommandMatcher.generateVMCommand("sub") shouldBe Left(Subtract)
    vMCommandMatcher.generateVMCommand("neg") shouldBe Left(Negative)
    vMCommandMatcher.generateVMCommand("not") shouldBe Left(Not)
    vMCommandMatcher.generateVMCommand("and") shouldBe Left(And)
    vMCommandMatcher.generateVMCommand("not") shouldBe Left(Not)
    vMCommandMatcher.generateVMCommand("eq") shouldBe Left(Equal)
    vMCommandMatcher.generateVMCommand("gt") shouldBe Left(GreaterThan)
    vMCommandMatcher.generateVMCommand("lt") shouldBe Left(LessThan)
  }

  it should "handle bad segment input" in {
    vMCommandMatcher.generateVMCommand("pop bad 4") shouldBe
      Right(ParsingError("Could not find segment: bad for pop bad 4."))
 }
}
