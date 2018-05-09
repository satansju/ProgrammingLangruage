package miniscala

import scala.util.parsing.input.{Position, Positional}

object Ast {

  /**
    * An AST node contains information about its position in the source code.
    */
  sealed abstract class AstNode extends Positional

  /**
    * Identifiers are just strings.
    */
  type Id = String

  /**
    * Expressions (excluding literals).
    */
  sealed abstract class Exp extends AstNode

  case class VarExp(x: Id) extends Exp

  case class BinOpExp(leftexp: Exp, op: BinOp, rightexp: Exp) extends Exp

  case class UnOpExp(op: UnOp, exp: Exp) extends Exp

  case class IfThenElseExp(condexp: Exp, thenexp: Exp, elseexp: Exp) extends Exp

  case class BlockExp(vals: List[ValDecl], vars: List[VarDecl], defs: List[DefDecl], classes: List[ClassDecl], exps: List[Exp]) extends Exp

  case class TupleExp(exps: List[Exp]) extends Exp

  case class MatchExp(exp: Exp, cases: List[MatchCase]) extends Exp

  case class CallExp(funexp: Exp, args: List[Exp]) extends Exp

  case class LambdaExp(params: List[FunParam], body: Exp) extends Exp

  case class AssignmentExp(x: Id, exp: Exp) extends Exp

  case class WhileExp(cond: Exp, body: Exp) extends Exp

  case class NewObjExp(klass: Id, args: List[Exp]) extends Exp

  case class LookupExp(objexp: Exp, member: Id) extends Exp

  /**
    * Literals.
    */
  sealed abstract class Literal extends Exp

  case class IntLit(c: Int) extends Literal

  case class BoolLit(c: Boolean) extends Literal

  case class FloatLit(c: Float) extends Literal

  case class StringLit(c: String) extends Literal

  case class NullLit() extends Literal

  /**
    * Binary operators.
    */
  sealed abstract class BinOp

  case object PlusBinOp extends BinOp

  case object MinusBinOp extends BinOp

  case object MultBinOp extends BinOp

  case object DivBinOp extends BinOp

  case object EqualBinOp extends BinOp

  case object LessThanBinOp extends BinOp

  case object LessThanOrEqualBinOp extends BinOp

  case object ModuloBinOp extends BinOp

  case object MaxBinOp extends BinOp

  case object AndBinOp extends BinOp

  case object OrBinOp extends BinOp

  /**
    * Unary operators.
    */
  sealed abstract class UnOp

  case object NegUnOp extends UnOp

  case object NotUnOp extends UnOp

  /**
    * Declarations.
    */
  sealed abstract class Decl extends AstNode

  case class ValDecl(x: Id, typean: TypeAnnotation, exp: Exp) extends Decl

  case class VarDecl(x: Id, typean: TypeAnnotation, exp: Exp) extends Decl

  case class DefDecl(fun: Id, params: List[FunParam], restypean: TypeAnnotation, body: Exp) extends Decl

  case class ClassDecl(klass: Id, params: List[FunParam], body: BlockExp) extends Decl

  /**
    * Function parameters.
    */
  case class FunParam(x: Id, typean: TypeAnnotation) extends AstNode

  /**
    * Match cases.
    */
  case class MatchCase(pattern: List[Id], exp: Exp) extends AstNode

  /**
    * Type annotations.
    */
  case class TypeAnnotation(opttype: Option[Type]) extends AstNode

  /**
    * Types.
    */
  sealed abstract class Type

  case object IntType extends Type

  case object BoolType extends Type

  case object FloatType extends Type

  case object StringType extends Type

  case class TupleType(types: List[Type]) extends Type

  case class FunType(paramtypes: List[Type], restype: Type) extends Type

  case class RefType(thetype: Type) extends Type // only used by TypeChecker

  case class ClassType(klass: Id) extends Type

  case class ClassDeclType(d: ClassDecl) extends Type // only used by Interpreter

  case class ConstructorType(d: ClassDecl, paramtypes: List[Type], membertypes: Map[Id, Type]) extends Type // only used by TypeChecker

  case object NullType extends Type

  /**
    * Exception with a message and a source code position,
    */
  abstract class MiniScalaError(msg: String, pos: Position) extends RuntimeException(s"$msg at line ${pos.line} column ${pos.column}\n${pos.longString}")
}
