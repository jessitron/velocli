package com.jessitron

import com.jessitron.Main.{Exit1, Fine}


class Test extends org.scalatest.FunSpec {
  describe("parsing options") {
    it("gives an error for two --options in a row") {
      val result = Main.parse(Seq("--one", "--two", "foo", "file"))
      result match {
        case Fine(_,_) => fail("This is not fine")
        case Exit1(errs) =>
          assert(errs.exists(_.contains("No value supplied for --one")), s"Errs were ${errs.mkString(" ")}")
      }
    }
    it("gives an error for two --options in a row with the file first") {
      val result = Main.parse(Seq("file", "--one", "--two", "foo"))
      result match {
        case Fine(_,_) => fail("This is not fine")
        case Exit1(errs) =>
          assert(errs.exists(_.contains("No value supplied for --one")), s"Errs were ${errs.mkString(" ")}")
      }
    }
    it("gives an error for both --options without values") {
      val result = Main.parse(Seq("file", "--one", "--two"))
      result match {
        case Fine(_,_) => fail("This is not fine")
        case Exit1(errs) =>
          assert(errs.exists(_.contains("No value supplied for --one")), s"Errs were ${errs.mkString(" ")}")
          assert(errs.exists(_.contains("No value supplied for --two")), s"Nothing for --two; Errs were ${errs.mkString(" ")}")
      }
    }
  }
}
