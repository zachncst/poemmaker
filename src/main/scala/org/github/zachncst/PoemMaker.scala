package org.github.zachncst

import fs2.{io, text, Task}
import scala.util.Either
import java.nio.file.Paths
import fs2.Stream
import scala.util.Random
import scala.annotation.tailrec

trait EmbedRegex {
  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
}

trait RuleElement {

}

case class Keyword(keyword : String) extends RuleElement

case class Text(str : String) extends RuleElement

case class RuleLink(rule : String) extends RuleElement

case class Randoms(segments : List[RuleElement]) extends RuleElement

case class Rule(name : String, segments : List[RuleElement])

class PoemMaker(rules : List[Rule]) extends EmbedRegex {
  val elementMap = rules.foldLeft(collection.mutable.Map[String, Rule]()) { (m, s) => m(s.name) = s; m }

  def makePoem() : String = {
    val poem = elementMap get "POEM"

    poem match {
      case Some(p) => convertRule(p)
      case None => throw new Exception("No POEM element in SCHEMA")
    }
  }

  def convertRule(r : Rule) = Stream.emits(r.segments)
    .map(convertElement)
    .intersperse(" ")
    .fold("")(_ + _)
    .toList
    .head

  def convertElement(ele : RuleElement) : String = {
    ele match {
      case Keyword(keyword) => {
        keyword match {
          case r"LINEBREAK" => "\n"
          case r"END" => ""
          case s => throw new Exception(s"Unrecognized keyword ${s}")
        }
      }
      case Text(str) => str
      case Randoms(segments) => {
        segments.lift(Random.nextInt(segments.size)) match {
          case Some(segment) => convertElement(segment)
          case None => throw new Exception("Random was not formatted correctly")
        }
      }
      case RuleLink(rule) => elementMap get rule match {
        case None => throw new Exception(s"Rule not found ${rule}")
        case Some(e) => convertRule(e)
      }
    }
  }
}

object PoemMaker extends EmbedRegex {
  def apply(rules : List[Rule]) = new PoemMaker(rules)

  def parseSchema(filepath : String) : PoemMaker = {
    io.file.readAll[Task](Paths.get(filepath), 4096)
      .through(text.utf8Decode)
      .through(text.lines)
      .map(toRule(_))
      .fold(List[Rule]())( (l, e) => l :+ e)
      .map(PoemMaker.apply(_))
      .runLog
      .unsafeRun()
      .head
  }

  def convertString(str: String) : RuleElement = {
    str.trim match {
      case r"(\w+)$text" => Text(text)
      case r"<(\w+)${link}>" => RuleLink(link)
      case r"\$$(\w+)${keyword}" => Keyword(keyword)
      case r"[\w\<\>\$$]+\|[\w\<\>\|\$$]+" => {
        Randoms(str.split("\\|").map(r => convertString(r)).toList)
      }
      case s => throw new Exception(s"Could not match string:${s}")
    }
  }

  def toRule(str : String) : Rule = {
    val sb1 = Stream.emits(str.split(" "))
    Rule(sb1.take(1).toList.head.replace(":", ""), sb1.drop(1).map(convertString).toList)
  }
}

object PoemMakerApp extends App {
  args headOption match {
    case Some(filename) => {
      val poemMaker = PoemMaker.parseSchema(args(0))
      println(poemMaker.makePoem())
    }
    case None => {
      throw new Exception("Please provide a filename as first arg")
    }
  }

}
