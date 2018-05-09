package miniscala

import miniscala.AbstractMachine._
import miniscala.Ast._

object Compiler {

  def compile(e: Exp): Executable = {

    case class IdDesc(x: Id, mutable: Boolean)

    def lookup(x: Id, idstack: List[IdDesc]): (IdIndex, Boolean) = {
      // find the position of identifier x in idstack
      val index = idstack.indexWhere(p => p.x == x)
      if (index == -1) throw new Exception(s"$x not found")
      // return the position and a boolean flag that indicates whether the identifier was declared with 'var'
      (index, idstack(index).mutable)
    }

    def compileFun(params: List[FunParam], body: Exp, freeids: List[Id], defs: List[DefDecl], idstack: List[IdDesc]) = {
      // prepare the new idstack for the function body, with an entry for each free non-def identifier, each def, and each parameter
      val defids = defs.map(d => d.fun).toSet
      val freenondefs = freeids.filterNot(defids.contains)
      val newidstack = freenondefs.map(x => IdDesc(x, lookup(x, idstack)._2)).reverse
      val newidstack2 = defs.foldLeft(newidstack)((ids, d) => IdDesc(d.fun, mutable = false) :: ids)
      val newidstack3 = params.foldLeft(newidstack2)((ids, p) => IdDesc(p.x, mutable = false) :: ids)
      // compile the function body
      val bodycode = compile(body, newidstack3.reverse, tailpos = true) ++ List(Return)
      // find idstack index for each free identifier (excluding defs in same block)
      val indices = freenondefs.map(x => lookup(x, idstack)._1)
      // produce a Lambda instruction
      List(Lambda(indices, bodycode))
    }

    def compile(e: Exp, idstack: List[IdDesc], tailpos: Boolean): List[Instruction] =
      e match {
        case IntLit(c) =>
          List(Const(c))
        case BoolLit(c) =>
          if(c) List(Const(1))
          else List(Const(0))
        case BinOpExp(leftexp, op, rightexp) =>
          compile(leftexp, idstack, tailpos = false) ++ compile(rightexp, idstack, tailpos = false) ++ List(op match {
            case PlusBinOp => Add
            case MinusBinOp => Sub
            case MultBinOp => Mul
            case DivBinOp => Div
            case EqualBinOp => Eq
            case LessThanBinOp => Lt
            case LessThanOrEqualBinOp => Leq
            case AndBinOp => And
            case OrBinOp => Or
            case _ => throw new CompilerError(e)
          })
        case UnOpExp(op, exp) =>
          compile(exp, idstack, tailpos = false) ++ List(op match {
            case NegUnOp => Neg
            case NotUnOp => Not
          })
        case IfThenElseExp(condexp, thenexp, elseexp) =>
          compile(condexp, idstack, tailpos = false) ++ List(Branch(compile(thenexp, idstack, tailpos), compile(elseexp, idstack, tailpos)))
        case WhileExp(cond, body) =>
          List(Loop(compile(cond, idstack, tailpos = false), compile(body, idstack, tailpos = false) ++ List(Pop)), Unit)
        case BlockExp(vals, vars, defs, _, exps) =>
          var stack = idstack
          var output: List[Instruction] = List()
          for(vl <- vals){
            output ++= compile(vl.exp, stack, tailpos = false) ++ List(Enter)
            stack = IdDesc(vl.x, mutable = false) :: stack
          }
          for(vr <- vars){
            output ++= List(Alloc, Dup) ++ compile(vr.exp, stack, tailpos = false) ++ List(Store, Enter)
            stack = IdDesc(vr.x, mutable = true) :: stack
          }
          for (df <- defs) {
            output ++= compileFun(df.params, df.body, Interpreter.freeVars(df).toList.sorted, defs, stack)
          }
          for(df <- defs){
            stack = IdDesc(df.fun, mutable = false) :: stack
          }
          output ++= List(EnterDefs(defs.length))
          for (exp <- exps.take(exps.length-1)){
            output ++= compile(exp, stack, tailpos = false) ++ List(Pop)
          }
          if(exps.nonEmpty)
            output ++= compile(exps.last, stack, tailpos)
          else
            output++= List(Unit)
          output ++ List(Exit(vals.size + vars.size + defs.size))

        case VarExp(x) =>
          val (name, mutable) = lookup(x, idstack)
          if(mutable)
            List(Read(name), Load)
          else
            List(Read(name))
        case AssignmentExp(x, exp) =>
          val (index, _) = lookup(x, idstack)
          Read(index) :: compile(exp, idstack, tailpos = false) ++ List(Store, Unit)
        case LambdaExp(params, body) =>
          compileFun(params, body, Interpreter.freeVars(e).toList.sorted, Nil, idstack)
        case CallExp(funexp, args) =>
          // compile funexp and args, and then add a Call instruction
          compile(funexp, idstack, tailpos = false) ++ args.flatMap(arg => compile(arg, idstack, tailpos = false)) ++ List(Call(args.length, tailpos))
        case _ => throw new CompilerError(e)
      }

    val freeids = Interpreter.freeVars(e).toList.sorted
    Executable(freeids, compile(e, freeids.map(x => IdDesc(x, mutable = false)), tailpos = true))
  }

  class CompilerError(node: AstNode) extends MiniScalaError(s"Sorry, I don't know how to compile $node", node.pos)
}
