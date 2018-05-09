package miniscala

import miniscala.Ast._

/**
  * Unparser for MiniScala.
  */
object Unparser {


  def unparse(n: AstNode): String =
    n match {
      case IntLit(c) => ""+c
      case FloatLit(c) => ""+c
      case StringLit(c) => c
      case BoolLit(c) => ""+c
      case BinOpExp(le, op, re) =>"(" + unparse(le) + unparse(op) + unparse(re) + ")"
      case UnOpExp(op, exp) =>  unparse(op) + unparse(exp)
      case BlockExp(vals, vars, defs, _, exp) => var output = "{"
        for(d <- vals){
          output +="val " + d.x + " = " + unparse(d.exp) + "; "
        }

        output += "("
        for(d <- defs){
          unparse(d)
          output += "def " + d.fun + "("
          for (p <- d.params){
            output += p.x + "" + unparse(p.typean) + ", "
          }
          output = output.substring(0,output.length()-2) + ")"
          output += "" + unparse(d.restypean) + " = {" + unparse(d.body) + "}"
        }
        output //+ unparse(exp) + "}; " //MERE SKAL TILFØJES

      case VarExp(x) => x
      case IfThenElseExp(ifexp, thenexp, elseexp) => "if(" + unparse(ifexp) +"){"+ unparse(thenexp)+ "} else {" + unparse(elseexp) + "}"
      case TupleExp(c) =>
        var output = "("
        for (d <- c) {
          output += unparse(d) + ", "
        }
        output = output.substring(0, output.length() - 2)
        output +")"
      case MatchExp(exp, cas) =>  "" + exp + " match {" + cas.map(unparse).mkString("; ") + "}"
      case ValDecl(x, typean, exp) => "val " + x + unparse(typean) + "=" + unparse(exp)
      case DefDecl(fun, params, restypean, body) =>
        var output = "def " + fun + "("
        for (p <- params){
          output += p.x + ": " + unparse(p.typean) + ", "
        }
        output = output.substring(0,output.length()-2) + ")"
        output + ": " + unparse(restypean) + " = {" + unparse(body) + "}"
      case CallExp(fun, args) => var output = "" + unparse(fun) + "("
        for(a <- args){
          output = output + unparse(a) + ", "
        }
        output.substring(0, output.length-2) + ")"

      case TupleExp(c) =>
        var output = "("
        for (t<-c)
          output += unparse(t)+", "
        output.substring(0, output.length-2) + ")"

      case LambdaExp(params, body) =>
        /*var string = "("
        for (par <- params){
          string += "(" + unparse(par) + ")"
        }
        string + ") => (" + unparse(body) + ")"*/
        params.foldRight("")((par, acc) => acc + "(" + unparse(par) + ")") + " => " + "(" + unparse(body) + ")"

      case FunParam(x, typean) =>
        x + unparse(typean)

      case TypeAnnotation(opt) =>
        opt match {
          case Some (t) => t match {
            case BoolType => ": Boolean"
            case IntType => ": Int"
            case FloatType => ": Float"
            case StringType => ": String"
            case TupleType(c) =>
              var output = ": ("
              for (t<-c)
                output += unparse(t)+", "
              output.substring(0, output.length-2) + ")"
          }
          case None => ""
        }
    }

  def unparse(op: BinOp): String =
    op match {
      case PlusBinOp => " + "
      case MinusBinOp => " - "
      case MultBinOp => " * "
      case DivBinOp => " / "
      case ModuloBinOp => " % "
      case MaxBinOp => " max "
      case EqualBinOp => " = "
      case AndBinOp => " & "
      case OrBinOp => " | "
      case LessThanBinOp => " < "
      case LessThanOrEqualBinOp => " <= "
    }

  def unparse(op: UnOp): String =
    op match {
      case NegUnOp => "-"
      case NotUnOp => "!"
    }

  def unparse(t: Type): String =
    t match {
      case BoolType => ": Boolean"
      case IntType => ": Int"
      case FloatType => ": Float"
      case StringType => ": String"
      case NullType => ": Null"
      case ClassDeclType(_) => ": Classtype"
      case n => ": " + n
    }
}
