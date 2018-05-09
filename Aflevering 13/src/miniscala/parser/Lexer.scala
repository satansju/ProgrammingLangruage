package miniscala.parser

import Parser.SyntaxError
import Tokens._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Position

/**
  * Lexer for MiniScala.
  *
  * (You do *not* need to read this code!)
  */
object Lexer extends RegexParsers {

  override def skipWhitespace = true

  override val whiteSpace: Regex = "([ \t\f]*(//.*[\r\n]+)?[\r\n]*)*".r

  def apply(code: String): List[MiniScalaToken] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => throw new SyntaxError(msg, getPosition(next))
      case Success(result, next) => result
    }
  }

  private def tokens: Parser[List[MiniScalaToken]] = {
    phrase(
      rep1(
        parensOpen |
          parensClose |
          blockOpen |
          blockClose |
          funArrow |
          literalBool |
          literalNull |
          iff |
          eelse |
          ofType |
          comma |
          mmatch |
          ccase |
          op |
          eq |
          cclass |
          nnew |
          dot |
          ttype |
          ddef |
          vval |
          vvar |
          wwhile |
          exprseq |
          identifier |
          literalString |
          literalFloat |
          literalInt
      )
    )
  }

  private def identifier: Parser[IDENTIFIER] = positioned {
    """[a-zA-Z_][a-zA-Z0-9_]*\b""".r ^^ { str =>
      IDENTIFIER(str)
    }
  }

  private def literalString: Parser[LITERAL_STRING] = positioned {
    """"[^"]*"""".r ^^ { lit =>
      val str = lit.substring(1, lit.length - 1)
      LITERAL_STRING(str)
    }
  }

  private def literalInt: Parser[LITERAL_INT] = positioned {
    """[0-9]+""".r ^^ { lit =>
      val v = lit.toInt
      LITERAL_INT(v)
    }
  }

  private def literalFloat: Parser[LITERAL_FLOAT] = positioned {
    """[0-9]+(\.[0-9]+)?f""".r ^^ { lit =>
      val v = lit.toFloat
      LITERAL_FLOAT(v)
    }
  }

  private def literalBool: Parser[LITERAL_BOOL] = positioned {
    """(true|false)\b""".r ^^ { lit =>
      val v = lit.toBoolean
      LITERAL_BOOL(v)
    }
  }

  private def literalNull: Parser[LITERAL_NULL] = positioned {
    "null" ^^ { lit =>
      LITERAL_NULL()
    }
  }

  private def op: Parser[OP] = positioned {
    """\+|\*|-|/|<=|==|<|%|!|\||&|max""".r ^^ { lit =>
      OP(lit)
    }
  }

  private def ttype: Parser[TTYPE] = positioned {
    """(String|Int|Float|Boolean|Unit|Null)\b""".r ^^ { lit =>
      TTYPE(lit)
    }
  }

  private def parensOpen = positioned {
    "(" ^^ { _ =>
      PARENS_OPEN()
    }
  }
  private def parensClose = positioned {
    ")" ^^ { _ =>
      PARENS_CLOSE()
    }
  }
  private def blockOpen = positioned {
    "{" ^^ { _ =>
      BLOCK_OPEN()
    }
  }
  private def blockClose = positioned {
    "}" ^^ { _ =>
      BLOCK_CLOSE()
    }
  }
  private def iff = positioned {
    """if\b""".r ^^ { _ =>
      IFF()
    }
  }
  private def eelse = positioned {
    """else\b""".r ^^ { _ =>
      EELSE()
    }
  }
  private def wwhile = positioned {
    """while\b""".r ^^ { _ =>
      WWHILE()
    }
  }
  private def eq = positioned {
    "=" ^^ { _ =>
      EQ()
    }
  }
  private def ofType = positioned {
    ":" ^^ { _ =>
      OFTYPE()
    }
  }
  private def comma = positioned {
    "," ^^ { _ =>
      COMMA()
    }
  }
  private def exprseq = positioned {
    ";" ^^ { _ =>
      EXPRSEQ()
    }
  }
  private def ddef = positioned {
    """def\b""".r ^^ { _ =>
      DDEF()
    }
  }
  private def vval = positioned {
    """val\b""".r ^^ { _ =>
      VVAL()
    }
  }
  private def vvar = positioned {
    """var\b""".r ^^ { _ =>
      VVAR()
    }
  }
  private def funArrow = positioned {
    "=>" ^^ { _ =>
      FUN_ARROW()
    }
  }
  private def mmatch = positioned {
    """match\b""".r ^^ { _ =>
      MATCH()
    }
  }
  private def ccase = positioned {
    """case\b""".r ^^ { _ =>
      CASE()
    }
  }

  private def cclass = positioned {
    """class\b""".r ^^ { _ =>
      CLASS()
    }
  }

  private def dot = positioned {
    "." ^^ { _ =>
      DOT()
    }
  }

  private def nnew = positioned {
    """new\b""".r ^^ { _ =>
      NEW()
    }
  }

  private def getPosition(i: Lexer.Input) = SyntheticPosition(i.pos.line, i.pos.column)

  private case class SyntheticPosition(line: Int, column: Int, lineContents: String = "") extends Position
}
