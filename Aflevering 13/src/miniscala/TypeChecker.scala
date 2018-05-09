package miniscala

import miniscala.Ast._
import miniscala.Unparser
import miniscala.Interpreter.RefVal

/**
  * Type checker for MiniScala.
  */
object TypeChecker {
  type TypeEnv = Map[Id, Type]
  type ClassTypeEnv = Map[Id, ConstructorType]
  val unit = TupleType(Nil) // just represent Unit as an empty tuple type


  def typeCheck(e: Exp, tenv: TypeEnv, ctenv: ClassTypeEnv): Type = e match {
    case IntLit(_) => IntType
    case BoolLit(_) => BoolType
    case FloatLit(_) => FloatType
    case StringLit(_) => StringType
    case NullLit() => NullType
    case VarExp(x) =>
      tenv.getOrElse(x, throw new TypeError(x + "unknown", e)) match {
        case RefType(t) => t
        case t => t
      }
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, tenv, ctenv)
      val righttype = typeCheck(rightexp, tenv, ctenv)
      op match {
        case PlusBinOp =>
          (lefttype, righttype) match {
            case (IntType, IntType) => IntType
            case (FloatType, FloatType) => FloatType
            case (IntType, FloatType) => FloatType
            case (FloatType, IntType) => FloatType
            case (StringType, StringType) => StringType
            case (StringType, IntType) => StringType
            case (StringType, FloatType) => StringType
            case (IntType, StringType) => StringType
            case (FloatType, StringType) => StringType
            case _ => throw new TypeError(s"Type mismatch at '+', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case MinusBinOp =>
          (lefttype, righttype) match {
            case (IntType, IntType) => IntType
            case (FloatType, FloatType) => FloatType
            case (IntType, FloatType) => FloatType
            case (FloatType, IntType) => FloatType
            case _ => throw new TypeError(s"Type mismatch at '-', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case MultBinOp =>
          (lefttype, righttype) match {
            case (IntType, IntType) => IntType
            case (FloatType, FloatType) => FloatType
            case (IntType, FloatType) => FloatType
            case (FloatType, IntType) => FloatType
            case (IntType, StringType) => StringType
            case _ => throw new TypeError(s"Type mismatch at '*', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case DivBinOp =>
          (lefttype, righttype) match {
            case (IntType, IntType) => IntType
            case (FloatType, FloatType) => FloatType
            case (IntType, FloatType) => FloatType
            case (FloatType, IntType) => FloatType
            case _ => throw new TypeError(s"Type mismatch at '/', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case ModuloBinOp =>
          (lefttype, righttype) match {
            case (IntType, IntType) => IntType
            case (FloatType, FloatType) => FloatType
            case (IntType, FloatType) => FloatType
            case (FloatType, IntType) => FloatType
            case _ => throw new TypeError(s"Type mismatch at '%', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case EqualBinOp =>
          (lefttype, righttype) match {
            case (IntType, IntType) => BoolType
            case (FloatType, FloatType) => BoolType
            case (IntType, FloatType) => BoolType
            case (FloatType, IntType) => BoolType
            case (BoolType, BoolType) => BoolType
            case (StringType, StringType) => BoolType
            case (TupleType(x), TupleType(y)) =>
              val xy = x.zip(y)
              for ((a, b) <- xy)
                checkSubtype(a, b, e)
              BoolType
            case _ => throw new TypeError(s"Type mismatch at '==', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case LessThanBinOp =>
          (lefttype, righttype) match {
            case (IntType, IntType) => BoolType
            case (FloatType, FloatType) => BoolType
            case (IntType, FloatType) => BoolType
            case (FloatType, IntType) => BoolType
            case _ => throw new TypeError(s"Type mismatch at '<', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case LessThanOrEqualBinOp =>
          (lefttype, righttype) match {
            case (IntType, IntType) => BoolType
            case (FloatType, FloatType) => BoolType
            case (IntType, FloatType) => BoolType
            case (FloatType, IntType) => BoolType
            case _ => throw new TypeError(s"Type mismatch at '<=', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case MaxBinOp =>
          (lefttype, righttype) match {
            case (IntType, IntType) => IntType
            case (FloatType, FloatType) => FloatType
            case (IntType, FloatType) => FloatType
            case (FloatType, IntType) => FloatType
            case _ => throw new TypeError(s"Type mismatch at 'Max', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case AndBinOp =>
          (lefttype, righttype) match {
            case (BoolType, BoolType) => BoolType
            case _ => throw new TypeError(s"Type mismatch at '&', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
        case OrBinOp =>
          (lefttype, righttype) match {
            case (BoolType, BoolType) => BoolType
            case _ => throw new TypeError(s"Type mismatch at '|', found types ${Unparser.unparse(lefttype)} and ${Unparser.unparse(righttype)}", e)
          }
      }
    case UnOpExp(op, exp) =>
      val ext = typeCheck(exp, tenv, ctenv)
      op match {
        case NegUnOp =>
          ext match {
            case IntType => IntType
            case FloatType => FloatType
            case _ => throw new TypeError(s"Type mismatch at 'if', found types ${Unparser.unparse(ext)}, expected BoolType", e)
          }
        case NotUnOp =>
          ext match {
            case BoolType => BoolType
            case _ => throw new TypeError(s"Type mismatch at 'if', found types ${Unparser.unparse(ext)}, expected BoolType", e)
          }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val condType = typeCheck(condexp, tenv, ctenv)
      if (condType != BoolType)
        throw new TypeError(s"Type mismatch at 'if', found types ${Unparser.unparse(condType)}, expected BoolType", e)
      val thenType = typeCheck(thenexp, tenv, ctenv)
      val elseType = typeCheck(elseexp, tenv, ctenv)
      (thenType, elseType) match {
        case (BoolType, BoolType) => BoolType
        case (IntType, IntType) => IntType
        case (FloatType, FloatType) => FloatType
        case (StringType, StringType) => StringType
        case _ => throw new TypeError(s"Type mismatch at 'then else', found types ${Unparser.unparse(thenType)} and ${Unparser.unparse(elseType)}", e)
      }
    case BlockExp(vals, vars, defs, classes, exp) =>
      var (tenv2, ctenv2) = (tenv, ctenv)
      for (d <- vars ++ vals ++ defs) {
        val thing = typeCheckDecl(d, tenv2, ctenv2, defs, classes)
        tenv2 = thing._1
        ctenv2 = thing._2
      }
      for (c <- classes)
        ctenv2 = typeCheckDecl(c, tenv2, ctenv2, defs, classes)._2
      if(exp.nonEmpty)
        typeCheck(exp.last, tenv2, ctenv2)
      else
        TupleType(List.empty)

    case TupleExp(exps) =>
      var lst = List[Type]()
      exps.foreach(ex => lst = lst ++ List(typeCheck(ex, tenv, ctenv)))
      TupleType(lst)
    case MatchExp(mexp, cases) =>
      val mexptype = typeCheck(mexp, tenv, ctenv)
      mexptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              var (tenv2, ctenv2) = (tenv, ctenv)
              val keyVals = c.pattern.zip(ts)
              for(x <- keyVals){
                tenv2 += (x._1 -> x._2)
              }
              return typeCheck(c.exp, tenv2, ctenv2)
            }
          }
          throw new TypeError(s"No case matches type ${Unparser.unparse(mexptype)}", e)
        case _ => throw new TypeError(s"Tuple expected at match, found ${Unparser.unparse(mexptype)}", e)
      }
    case CallExp(funexp, args) =>
      typeCheck(funexp, tenv, ctenv) match{
        case FunType(params, resType) =>
          if (params.length!=args.length) throw new TypeError(s"Not enough arguments ${Unparser.unparse(e)}", e)
          for((p, a) <- params.zip(args)){
            if (typeCheck(a, tenv, ctenv) != p) throw new TypeError(s"Arguments not matching ${Unparser.unparse(p)}", e)
          }
          resType
        case x => throw new TypeError(s"Expected function, found ${Unparser.unparse(x)}", e)
      }

    case LambdaExp(params, body) =>
      var lst: List[Type] = List()
      var tenv2 = tenv
      for(param <- params){
        val typ = param.typean.opttype.getOrElse(throw new TypeError(s"add typeannotation ${Unparser.unparse(param)}", e))
        lst = lst ++ List(typ)
        tenv2 = tenv2 + (param.x -> typ)
      }
      FunType(lst, typeCheck(body, tenv2, ctenv))

    case AssignmentExp(x, exp) =>
      tenv.getOrElse(x, throw new TypeError(s"$x is not a known anything", e)) match {
        case RefType(t) =>
          val t2 = typeCheck(exp, tenv, ctenv)
          checkSubtype(t2,t,e)
          /*f (!subtype(t2, t))
            throw new TypeError(s"Type mismatch: expected type ${Unparser.unparse(t)}, found type ${Unparser.unparse(t2)}", e)*/
          TupleType(List.empty)
        case n =>println(n); throw new TypeError(s"$x is a val, not a var", e)
      }
    case WhileExp(cond, body) =>
      val condtype = typeCheck(cond, tenv, ctenv)
      if (condtype != BoolType)
        throw new TypeError(s"Type mismatch: expected type ${Unparser.unparse(BoolType)}, found type ${Unparser.unparse(condtype)}", e)
      typeCheck(body, tenv, ctenv)
      TupleType(List.empty)
    case NewObjExp(klass, args) =>
      val typ = ctenv.getOrElse(klass, throw new TypeError("Unknown klass name", e))
      if(typ.paramtypes.size != args.size) throw new TypeError("Incorrect number of parameters", e)
      for((p,a) <- typ.paramtypes.zip(args)){
        checkSubtype(getType(typeCheck(a, tenv, ctenv), ctenv, e), Some(p), e)
      }
      typ
    case LookupExp(objexp, member) =>
      val typ = typeCheck(objexp,tenv, ctenv)
      typ match {
        case c @ ConstructorType(d, par, mems) =>
          mems.getOrElse(member, throw new TypeError("no such member in " + c, e))
        case _ => throw new TypeError("type missmatch", e)
      }

  }

  def typeCheckDecl(d: Decl, tenv: TypeEnv, ctenv: ClassTypeEnv, defs: List[DefDecl], classes: List[ClassDecl]): (TypeEnv, ClassTypeEnv) = d match {
    case ValDecl(x, typean, exp) =>
      val t = typeCheck(exp, tenv, ctenv)
      checkSubtype(t, getType(typean.opttype, ctenv, d), d)
      (tenv + (x -> typean.opttype.getOrElse(t)), ctenv)

    case VarDecl(x, typean, exp) =>
      val exptype = typeCheck(exp, tenv, ctenv)
      checkSubtype(exptype, getType(typean.opttype,ctenv,d), d)
      (tenv + (x -> RefType(typean.opttype.getOrElse(exptype))),ctenv)

    case d1 @ DefDecl(fun, params, restypean, exp) =>
      var ntenv = tenv
      val funType = getFunType(d1, ctenv)
      for(d <- defs){
        val ftype = getFunType(d, ctenv)
        ntenv += (d.fun -> FunType(ftype.paramtypes, ftype.restype))
      }
      for((p,e) <- d1.params.zip(funType.paramtypes)) {
        ntenv += (p.x -> e)
        checkSubtype(e, p.typean.opttype, d)
      }
      checkSubtype(typeCheck(exp, ntenv, ctenv), funType.restype, d1)
      (ntenv, ctenv)

    case d2 @ ClassDecl(klass, params, body) =>
      var membertyps: Map[Id, Type] = Map.empty

      var inctenv = ctenv
      for(p <- params) {
        membertyps += (p.x -> p.typean.opttype.getOrElse(throw new TypeError(s"${p.x} needs typeannotations", d2)))
      }
      for(dl <- body.vals ++ body.vars ++ body.defs){
        membertyps = typeCheckDecl(dl, membertyps, inctenv, body.defs, body.classes)._1
      }
      for(cl <- body.classes) {
        inctenv = typeCheckDecl(cl, membertyps, inctenv, body.defs, body.classes)._2
        membertyps = typeCheckDecl(cl, membertyps, inctenv, body.defs, body.classes)._1
      }
      body.exps.foreach(e => typeCheck(e, membertyps, inctenv))
      val nctenv = ctenv + (klass -> ConstructorType(d2, params.map(p=>getType(p.typean.opttype.get, ctenv, d2)), membertyps))

      (tenv, nctenv)
  }

  def getType(t: Type, ctenv: ClassTypeEnv, n: AstNode): Type = t match {
    case ClassType(klass) => ctenv.getOrElse(klass, throw new TypeError(s"Unknown class '$klass'", n))
    case IntType | BoolType | FloatType | StringType => t
    case TupleType(ts) => TupleType(ts.map(tt => getType(tt, ctenv, n)))
    case FunType(paramtypes, restype) => FunType(paramtypes.map(tt => getType(tt, ctenv, n)), getType(restype, ctenv, n))
    case _ => println(t); throw new TypeError(s"Unexpected type $t",n) // this case is unreachable...
  }

  def getType(ot: Option[Type], ctenv: ClassTypeEnv, n: AstNode): Option[Type] = ot.map(t => getType(t, ctenv, n))

  def getFunType(d: DefDecl, ctenv: ClassTypeEnv): FunType =
    FunType(d.params.map(p => getType(p.typean.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", d)), ctenv, p)),
      getType(d.restypean.opttype.getOrElse(throw new TypeError(s"Type annotation missing at function result ${d.fun}", d)), ctenv, d))

  def subtype(t1: Type, t2: Type): Boolean = {
    if(t1 == t2)  return true
    else {
      (t1,t2) match {
        case (IntType, FloatType) => true
        case (NullType, _) => true
        //case (_, NullType) => true
        case (FunType(paramtypes1, restype1), FunType(paramtypes2, restype2)) =>
          var res = true
          for((p1,p2) <- (paramtypes1.zip(paramtypes2))){
            if (!subtype(p2, p1))
              res = false
          }
          subtype(restype1, restype2) & res
        case (TupleType(ts1), TupleType(ts2)) =>
          for((t1, t2) <- ts1.zip(ts2)){
            if(!subtype(t2, t1))
              return false
          }
          true
        case (_, _) => false
      }
    }
  }

  def checkSubtype(t1: Type, t2: Type, n: AstNode): Unit =
    if (!subtype(t1, t2)) throw new TypeError(s"Type mismatch: type ${Unparser.unparse(t1)} is not subtype of ${Unparser.unparse(t2)}", n)

  def checkSubtype(t: Type, ot: Option[Type], n: AstNode): Unit = ot.foreach(t2 => checkSubtype(t, t2, n))


  def checkTypesEqual(t: Type, ot: Option[Type], n: AstNode): Unit = ot match {
    case Some(t2) =>
      if (t != t2){
        throw new TypeError(s"Type mismatch: expected type ${Unparser.unparse(t2)}, found type ${Unparser.unparse(t)}", n)}
    case None => // do nothing
  }


  /**
    * Builds an initial type environment, with a type for each free variable in the program.
    */
  def makeInitialTypeEnv(program: Exp): TypeEnv = {
    var tenv: TypeEnv = Map()
    for (x <- Interpreter.freeVars(program))
      tenv = tenv + (x -> IntType)
    tenv
  }

  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)
}
