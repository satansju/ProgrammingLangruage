package miniscala.parser

import java.io.File

import miniscala.Ast._
import Tokens._

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input._

/**
  * Parser for MiniScala.
  *
  * (You do *not* need to read this code!)
  */
object Parser extends Parsers {

  override type Elem = MiniScalaToken

  private def prog: Parser[Exp] = expr()

  private def expr(antiPrecedence: Int = 8): Parser[Exp] = memoized(s"expr($antiPrecedence)") {
    positioned {
      antiPrecedence match {
        case 8 =>
          ifthenelse |
            wwhile |
            assignment |
            lambda |
            expr(7)
        case 7 =>
          mmatch |
            expr(6)
        case x if x >= 0 =>
          binopexp(antiPrecedence) |
            expr(x - 1)
        case -1 =>
          call(Context.None) |
            tupleexp |
            expr(-2)
        case -2 =>
          lookup |
            expr(-3)
        case -3 =>
          newobj |
            unaryexp |
            literals |
            block |
            parens
      }
    }
  }

  private def parens = positioned {
    (PARENS_OPEN() ~ expr() ~ PARENS_CLOSE()) ^^ { case _ ~ exp ~ _ => exp }
  }

  private def mmatch = positioned {
    (expr(6) ~ MATCH() ~ BLOCK_OPEN() ~ repsep(ccase, EXPRSEQ()) ~ BLOCK_CLOSE()) ^^ {
      case target ~ _ ~ _ ~ cases ~ _ =>
        MatchExp(target, cases)
    }
  }

  private def tupleexp = positioned {
    (PARENS_OPEN() ~ expr() ~ COMMA() ~ repsep(expr(), COMMA()) ~ PARENS_CLOSE()) ^^ { case _ ~ exp ~ _ ~ others ~ _ => TupleExp(exp :: others) } |
      (PARENS_OPEN() ~ PARENS_CLOSE()) ^^ { case _ ~ _ => TupleExp(List()) }
  }

  private def ccase = positioned {
    CASE() ~ tuplepattern ~ FUN_ARROW() ~ expr() ^^ {
      case _ ~ matcher ~ _ ~ body =>
        MatchCase(matcher, body)
    }
  }

  private def tuplepattern = {
    (PARENS_OPEN() ~ repsep(identifier, COMMA()) ~ PARENS_CLOSE()) ^^ {
      case _ ~ ids ~ _ => ids.map(_.str)
    }
  }

  private def ifthenelse = positioned {
    (IFF() ~ PARENS_OPEN() ~ expr() ~ PARENS_CLOSE() ~ expr() ~ EELSE() ~ expr()) ^^ {
      case _ ~ _ ~ exp1 ~ _ ~ exp2 ~ _ ~ exp3 => IfThenElseExp(exp1, exp2, exp3)
    }
  }

  private def wwhile = positioned {
    (WWHILE() ~ PARENS_OPEN() ~ expr() ~ PARENS_CLOSE() ~ expr()) ^^ {
      case _ ~ _ ~ exp1 ~ _ ~ exp2 => WhileExp(exp1, exp2)
    }
  }

  private def blockel = valdecl | vardecl | defdecl | classdecl | expr()

  private def blockelmseq = repsep(blockel, EXPRSEQ())

  type BlockTupleType = Tuple5[List[ValDecl], List[VarDecl], List[DefDecl], List[ClassDecl], List[Exp]]

  private def validBlock[T](l: List[T]): Option[BlockTupleType] = {
    val matchers = List[Function[T, Boolean]](
      { case _: ValDecl => true
      case _ => false
      },
      { case _: VarDecl => true
      case _ => false
      },
      { case _: DefDecl => true
      case _ => false
      },
      { case _: ClassDecl => true
      case _ => false
      },
      { case _: Exp => true
      case _ => false
      })
    val (remaining, splits) = matchers.foldLeft((l, List[List[T]]())) { case ((list: List[T], outcome: List[List[T]]), matcher) =>
      val sublist = list.takeWhile(elm => matcher(elm))
      (list.drop(sublist.size), sublist :: outcome)
    }
    val items = splits.reverse
    if (remaining.isEmpty) Some((
      items(0).map(_.asInstanceOf[ValDecl]),
      items(1).map(_.asInstanceOf[VarDecl]),
      items(2).map(_.asInstanceOf[DefDecl]),
      items(3).map(_.asInstanceOf[ClassDecl]),
      items(4).map(_.asInstanceOf[Exp]))
    )
    else None
  }

  private def block = positioned {
    ((BLOCK_OPEN() ~ blockelmseq ~ BLOCK_CLOSE()) ^^ {case _ ~ l ~ _ => validBlock(l).map(BlockExp.tupled) } filter(_.isDefined)) ^^ {_.get}
  }

  private def assignment = positioned {
    (identifier ~ EQ() ~ expr()) ^^ {
      case id ~ _ ~ exp => AssignmentExp(id.str, exp)
    }
  }

  private def call(context: Context.Value) = positioned {
    context match {
      case Context.Lookup =>
        (identifier ~ rep1(appl)) ^^ { case target ~ applications => applications.tail.foldLeft(CallExp(VarExp(target.str), applications.head)) { case (curr, acc) => CallExp(curr, acc) } }
      case _ =>
        (expr(-2) ~ rep1(appl)) ^^ { case target ~ applications => applications.tail.foldLeft(CallExp(target, applications.head)) { case (curr, acc) => CallExp(curr, acc) } }
    }
  }

  private def appl = (PARENS_OPEN() ~ repsep(expr(), COMMA()) ~ PARENS_CLOSE()) ^^ { case _ ~ apps ~ _ => apps }

  private def lambda = positioned {
    PARENS_OPEN() ~ repsep(identifier ~ opttypeannotation, COMMA()) ~ PARENS_CLOSE() ~ FUN_ARROW() ~ expr() ^^ {
      case _ ~ identifiers ~ _ ~ _ ~ body =>
        LambdaExp(identifiers.map(p => FunParam(p._1.str, p._2)), body)
    }
  }

  private def replaceVarTarget(callExp: CallExp, newTarget: VarExp => Exp): CallExp = {
    callExp match {
      case CallExp(id: VarExp, a) => CallExp(newTarget(id), a)
      case CallExp(e: CallExp, a) => CallExp(replaceVarTarget(e, newTarget), a)
      case _ => ???
    }
  }

  private def lookup = positioned {
    (call(Context.Lookup) | expr(-3)) ~ DOT() ~ rep1sep(call(Context.Lookup) | identifier, DOT()) ^^ { case e ~ _ ~ ids => ids.foldLeft(e: Exp) { case (acc, curr) =>
      curr match {
        case c: CallExp => replaceVarTarget(c, id => LookupExp(acc, id.x))
        case id: IDENTIFIER => LookupExp(acc, id.str)
      }
    }
    }
  }

  private def newobj = positioned {
    NEW() ~ identifier ~ appl ^^ { case _ ~ name ~ args => NewObjExp(name.str, args)}
  }

  private def valdecl = positioned {
    (VVAL() ~ identifier ~ opttypeannotation ~ EQ() ~ expr()) ^^ {
      case _ ~ id ~ t ~ _ ~ exp =>
        ValDecl(id.str, t, exp)
    }
  }

  private def vardecl = positioned {
    (VVAR() ~ identifier ~ opttypeannotation ~ EQ() ~ expr()) ^^ {
      case _ ~ id ~ t ~ _ ~ exp =>
        VarDecl(id.str, t, exp)
    }
  }

  private def defdecl = positioned {
    (DDEF() ~ identifier ~ PARENS_OPEN() ~ repsep(identifier ~ opttypeannotation, COMMA()) ~ PARENS_CLOSE() ~ opttypeannotation ~ EQ() ~ expr()) ^^ {
      case _ ~ id ~ _ ~ identifiers ~ _ ~ retType ~ _ ~ exp =>
        DefDecl(id.str, identifiers.map(p => FunParam(p._1.str, p._2)), retType, exp)
    }
  }

  private def classdecl: Parser[ClassDecl] = positioned {
    CLASS() ~ identifier ~ PARENS_OPEN() ~ repsep(identifier ~ opttypeannotation, COMMA()) ~ PARENS_CLOSE() ~ block ^^ { case _ ~ name ~ _ ~ params ~ _ ~ body =>
      ClassDecl(name.str, params.map(p => FunParam(p._1.str, p._2)), body)
    }
  }

  private def opttypeannotation = positioned {
    opt { (OFTYPE() ~ typeannotation) ^^ {case _ ~ ta => ta}  } ^^ {ta => TypeAnnotation(ta)}
  }

  private def binopexp(antiPrecedence: Int) = positioned {
    expr(antiPrecedence - 1) * {
      binop(antiPrecedence) ^^ { op => { (left: Exp, right: Exp) => BinOpExp(left, op, right) } }
    }
  }

  private def literals = positioned {
    strliteral ^^ { lit => StringLit(lit.str) } |
      boolliteral ^^ { lit => BoolLit(lit.b) } |
      intliteral ^^ { lit => IntLit(lit.i) } |
      floatliteral ^^ { lit => FloatLit(lit.v) } |
      nullliteral ^^ { lit => NullLit() } |
      identifier ^^ { id => VarExp(id.str) }
  }

  private def unaryexp = positioned {
    (notop ~ expr(-1)) ^^ { case op ~ exp => UnOpExp(NotUnOp, exp) } |
      (negop ~ expr(-1)) ^^ { case op ~ exp => UnOpExp(NegUnOp, exp) }
  }

  private def typeannotation: Parser[Type] = complextype ^? {case Left(t) => t}

  type TypeOrList = Either[Type, TupleType]

  private def complextype: Parser[TypeOrList] = {
    rep1sep(nonfuntype, FUN_ARROW()) ^^ { items =>
      val revItems = items.reverse
      revItems.tail.foldLeft(revItems.head) { case (r, l) =>
        val left = l match {
          case Left(TupleType(lvs)) => lvs
          case Left(x) => List(x)
          case Right(lvs) => List(lvs)
        }
        val right = r match {
          case Left(rv) => rv
          case Right(rvs) => rvs
        }
        Left(FunType(left, right))
      }
    }
  }

  private def nonfuntype: Parser[TypeOrList] = {
    ttype ^^ { t => t.str match {
      case "Int" => Left(IntType)
      case "String" => Left(StringType)
      case "Boolean" => Left(BoolType)
      case "Float" => Left(FloatType)
      case "Unit" => Left(TupleType(List()))
      case "Null" => Left(NullType)
    }
    } |
      (PARENS_OPEN() ~ PARENS_CLOSE()) ^^ { case _ ~ _ => Left(TupleType(List())) } |
      (PARENS_OPEN() ~ typeannotation ~ COMMA() ~ rep1sep(typeannotation, COMMA()) ~ PARENS_CLOSE()) ^^ { case (_ ~ t0 ~ _ ~ ts ~ _) => Left(TupleType(t0 :: ts)) } |
      (PARENS_OPEN() ~ complextype ~ PARENS_CLOSE()) ^^ { case (_ ~ t ~ _) => t match {
        case Left(TupleType(it)) => Right(TupleType(it))
        case Right(x) => Right(x)
        case Left(x) => Left(x)
      } } |
      identifier ^^ (id => Left(ClassType(id.str)))
  }

  private def binop(antiPrecedence: Int): Parser[BinOp] = {
    antiPrecedence match {
      case 0 =>
        mult | div | modulo
      case 1 =>
        plus | minus
      case 2 =>
        equalequal
      case 3 =>
        lt | lteq
      case 4 =>
        and
      case 5 =>
        or
      case 6 =>
        max
    }
  }

  private def ttype: Parser[TTYPE] = positioned {
    accept("type", { case t@TTYPE(v) => t })
  }

  private def strliteral: Parser[LITERAL_STRING] = positioned {
    accept("string literal", { case lit: LITERAL_STRING => lit })
  }

  private def intliteral: Parser[LITERAL_INT] = positioned {
    accept("int literal", { case lit: LITERAL_INT => lit })
  }

  private def boolliteral: Parser[LITERAL_BOOL] = positioned {
    accept("boolean literal", { case lit: LITERAL_BOOL => lit })
  }

  private def floatliteral: Parser[LITERAL_FLOAT] = positioned {
    accept("float literal", { case lit: LITERAL_FLOAT => lit })
  }

  private def nullliteral: Parser[LITERAL_NULL] = positioned {
    accept("null literal", { case lit: LITERAL_NULL => lit })
  }

  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id@IDENTIFIER(name) => id })
  }

  private def plus = accept("plusop", { case OP("+") => PlusBinOp })

  private def minus = accept("minusop", { case OP("-") => MinusBinOp })

  private def mult = accept("multop", { case OP("*") => MultBinOp })

  private def div = accept("divop", { case OP("/") => DivBinOp })

  private def equalequal = accept("eqeqop", { case OP("==") => EqualBinOp })

  private def and = accept("and", { case OP("&") => AndBinOp })

  private def or = accept("or", { case OP("|") => OrBinOp })

  private def max = accept("max", { case OP("max") => MaxBinOp })

  private def lt = accept("ltop", { case OP("<") => LessThanBinOp })

  private def modulo = accept("modulo", { case OP("%") => ModuloBinOp })

  private def lteq = accept("gteqop", { case OP("<=") => LessThanOrEqualBinOp })

  private def negop = accept("negop", { case OP("-") => NegUnOp })

  private def notop = accept("notop", { case OP("!") => NotUnOp })

  def setPos[E <: Positional](n: E, p: Position): E = {
    n.setPos(p)
    n
  }

  private def parseTokens(tokens: Seq[MiniScalaToken]): Exp = {
    val reader = new MiniScalaTokenReader(tokens)
    prog(reader) match {
      case NoSuccess(msg, next) =>
        throw new SyntaxError(msg, next.pos)
      case Success(result, next) if !next.atEnd =>
        throw new SyntaxError("Input not fully read", next.pos)
      case Success(result, next) =>
        result
    }
  }

  def parse(code: String): Exp = {
    val tokens = Lexer(code)
    try {
      parseTokens(tokens)
    }
    finally {
      memo.clear()
    }
  }

  def readFile(path: String): String = {
    Source.fromFile(new File(path)).mkString
  }

  object Context extends Enumeration {
    val None, Lookup = Value
  }

  class SyntaxError(msg: String, pos: Position) extends MiniScalaError(s"Syntax error: $msg", pos)

  class MiniScalaTokenReader(tokens: Seq[MiniScalaToken]) extends Reader[MiniScalaToken] {
    override def first: MiniScalaToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[MiniScalaToken] = new MiniScalaTokenReader(tokens.tail)
  }

  type P = (Int, Int)

  val DISABLE_MEMOIZATION = false

  var memo: mutable.Map[(String, P), (_, Input)] = mutable.Map()

  def memoized[T](name: String)(p: => Parser[T]): Parser[T] = {
    if (DISABLE_MEMOIZATION) {
      p
    } else {
      Parser { in =>
        val startPos = (in.pos.line, in.pos.column)
        val key = (name, startPos)
        memo.get(key) match {
          case Some(x) =>
            memo.remove(key)
            Success(x._1.asInstanceOf[T], x._2)
          case None => Failure("memoization failed", in)
        }
      } |
        Parser { in =>
          val startPos = (in.pos.line, in.pos.column)
          val key = (name, startPos)
          p(in) match {
            case s@Success(t, rest) =>
              memo.put(key, (t, rest))
              s
            case ns: NoSuccess => ns
          }
        }
    }
  }
}
