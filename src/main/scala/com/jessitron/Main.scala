package com.jessitron

import java.io.{File, StringWriter}

import org.apache.velocity.app.VelocityEngine
import org.apache.velocity.{Template, VelocityContext}

/**
  * Created by jessitron on 6/26/16.
  */
object Main {
  def main(args: Array[String]) {
    val USAGE = "Usage: velocli <template_file> [--parameter value]..."

    parse(args) match {
      case Exit1(errors) =>
        println(USAGE)
        errors.foreach(System.err.println)
        System.exit(1)
      case Fine(templateFilename, parameters) =>
        val ve: VelocityEngine = new VelocityEngine
        ve.init
        val t: Template = ve.getTemplate(templateFilename)
        val context: VelocityContext = new VelocityContext

        parameters.foreach { case (k, v) =>
          println(s"Setting $k to $v")
          context.put(k, v)
        }
        val writer: StringWriter = new StringWriter
        t.merge(context, writer)
        System.out.println(writer.toString)
    }
  }

  sealed trait ParseInstruction

  case class Exit1(printToStderr: Seq[String]) extends ParseInstruction

  case class Fine(filename: String, parameters: Map[String, String]) extends ParseInstruction

  def parse(args: Seq[String]): ParseInstruction = {
    something(Stuff(args)) match {
      case Stuff(_, errors, _, _) if errors.nonEmpty =>
        Exit1(errors)
      case Stuff(_, _, unlabeledArgs, _) if unlabeledArgs.isEmpty =>
        Exit1(Seq("Please supply a template filename"))
      case Stuff(_, _, unlabeledArgs, _) if unlabeledArgs.size > 1 =>
        Exit1(Seq("Please supply only one template filename",
          s"(You supplied ${unlabeledArgs.mkString(" and ")}"))
      case Stuff(_, _, Seq(templateFilename), _)
        if !new File(templateFilename).exists() =>
        Exit1(Seq(s"File $templateFilename not found."))
      case Stuff(_, _, Seq(templateFilename), parameters) =>
        Fine(templateFilename, parameters)
    }
  }

  type FailureDescription = String

  case class Stuff(rest: Seq[String],
                   errors: Seq[FailureDescription] = Seq(),
                   unlabeledArgs: Seq[String] = Seq(),
                   parameters: Map[String, String] = Map()
                  )

  def something(stuff: Stuff): Stuff = {
    stuff.rest match {
      case Seq() => stuff
      case Seq(one) if one.startsWith("--") =>
        stuff.copy(errors = stuff.errors :+ s"No value supplied for $one")
      case Seq(next, value, tail@_*) if next.startsWith("--") =>
        if (value.startsWith("--")) {
          something(stuff.copy(errors = stuff.errors :+ s"No value supplied for $next", rest = value +: tail))
        } else {
          val parameter = next.substring(2)
          val moreParameters = stuff.parameters + (parameter -> value)
          something(stuff.copy(parameters = moreParameters, rest = tail))
        }
      case Seq(next, rest@_*) =>
        // maybe this is the filename
        something(stuff.copy(unlabeledArgs = stuff.unlabeledArgs :+ next, rest = rest))
    }
  }

}
