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
    val Stuff(_, errors, unlabeledArgs, parameters) = something(Stuff(args))
    if (errors.nonEmpty) {
      println(USAGE)
      errors.foreach(System.err.println)
      System.exit(1)
    }
    if (unlabeledArgs.isEmpty) {
      println(USAGE)
      System.err.println("Please supply a template filename")
      System.exit(1)
    }
    if (unlabeledArgs.size > 1) {
      println(USAGE)
      System.err.println("Please supply only one template filename")
      System.err.println(s"(You supplied ${unlabeledArgs.mkString(" and ")}")
      System.exit(1)
    }

    val templateFilename: String = unlabeledArgs.head

    if (!new File(templateFilename).exists()) {
      System.err.println(s"File $templateFilename not found.")
      System.exit(1)
    }
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

  type FailureDescription = String

  case class Stuff(rest: Seq[String],
                   errors: Seq[FailureDescription] = Seq(),
                   unlabeledArgs: Seq[String] = Seq(),
                   parameters: Map[String, String] = Map()
                  )

  def something(stuff: Stuff): Stuff = {
    import stuff._
    if (rest.isEmpty) {
      stuff
    } else {
      val next = rest.head
      if (next.startsWith("--")) {
        val parameter = next.substring(2)
        if (rest.tail.isEmpty) {
          stuff.copy(errors = errors :+ s"No value supplied for ${parameter}")
        }
        val moreParameters = parameters + (parameter -> rest.tail.head)
        something(stuff.copy(parameters = moreParameters, rest = rest.tail.tail))
      } else {
        // maybe this is the filename
        something(stuff.copy(unlabeledArgs = unlabeledArgs :+ next, rest = rest.tail))
      }
    }
  }
}
