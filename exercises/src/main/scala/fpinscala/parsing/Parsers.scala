package fpinscala.parsing

import fpinscala.testing._

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = {
    string(c.toString) map (_.charAt(0))
  }

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def succeed[A](a: A): Parser[A] = {
    string("") map (_ => a)
  }
  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = {
    flatMap(p)(a => p2.map(b => (a, b)))
  }

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = {
    flatMap(p)(a => p2.map(b => f(a, b)))
  }

  def nC(c: Char): Parser[List[Char]] = {
    flatMap(regex("[0-9]+".r))(n => listOfN(n.toInt, char(c)))
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  //implicits
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  def nA(p: Parser[String])

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def listOfN(n: Int): Parser[List[A]] = ???
    def map[B](f: A => B): Parser[B] = {
      flatMap(p)(a => succeed(f(a)))
    }
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

  def leftCurly(): Parser[String] = {
    string("{")
  }

  def rightCurly(): Parser[String] = {
    string("}")
  }

  def leftBracket(): Parser[String] = {
    string("[")
  }

  def rightBracket(): Parser[String] = {
    string("]")
  }

  def comma(): Parser[String] = {
    string(",")
  }

  def space(): Parser[String] = {
    string(" ")
  }

  def stringVal(): Parser[String] = {
    regex(raw"([\"'])(?:(?=(\\?))\2.)*?\1".r)
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends  JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}