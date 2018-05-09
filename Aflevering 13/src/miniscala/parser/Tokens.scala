package miniscala.parser

import scala.util.parsing.input.Positional

/**
  * Tokens used by the MiniScala lexer and parser.
  *
  * (You do *not* need to read this code!)
  */
object Tokens {

  sealed trait MiniScalaToken extends Positional

  case class INDENT(spaces: Int) extends MiniScalaToken

  case class OP(str: String) extends MiniScalaToken

  case class IDENTIFIER(str: String) extends MiniScalaToken

  case class TTYPE(str: String) extends MiniScalaToken

  case class LITERAL_STRING(str: String) extends MiniScalaToken

  case class LITERAL_INT(i: Int) extends MiniScalaToken

  case class LITERAL_BOOL(b: Boolean) extends MiniScalaToken

  case class LITERAL_FLOAT(v: Float) extends MiniScalaToken

  case class LITERAL_NULL() extends MiniScalaToken

  case class VVAL() extends MiniScalaToken

  case class VVAR() extends MiniScalaToken

  case class DDEF() extends MiniScalaToken

  case class WWHILE() extends MiniScalaToken

  case class IFF() extends MiniScalaToken

  case class EELSE() extends MiniScalaToken

  case class PARENS_OPEN() extends MiniScalaToken

  case class PARENS_CLOSE() extends MiniScalaToken

  case class BLOCK_OPEN() extends MiniScalaToken

  case class BLOCK_CLOSE() extends MiniScalaToken

  case class EQ() extends MiniScalaToken

  case class OFTYPE() extends MiniScalaToken

  case class EOP() extends MiniScalaToken

  case class COMMA() extends MiniScalaToken

  case class EXPRSEQ() extends MiniScalaToken

  case class FUN_ARROW() extends MiniScalaToken

  case class MATCH() extends MiniScalaToken

  case class CASE() extends MiniScalaToken

  case class CLASS() extends MiniScalaToken

  case class DOT() extends MiniScalaToken

  case class NEW() extends MiniScalaToken
}
