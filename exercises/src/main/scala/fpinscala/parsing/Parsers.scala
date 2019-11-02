package fpinscala.parsing

import fpinscala.testing._

import language.higherKinds

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
  def many1[A](P: Parser[A]): Parser[List[A]]
  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = {
    product(p, p2).map(p => f(p._1, p._2))
  }

  //implicits
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))



  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)


    def many[B](f: A => B): Parser[B] = ???
    def listOfN(n: Int): Parser[List[A]] = ???
    def map[B](f: A => B): Parser[B] = ???
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.many(a => a))(in)
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