package miniscala

import miniscala.Ast._

import scala.io.StdIn

/**
  * Interpreter for MiniScala.
  */
object Interpreter {

  def freeVars(e: Exp): Set[Id] = e match {
    case _: Literal => Set()
    case VarExp(x) => Set(x)
    case BinOpExp(leftexp, _, rightexp) => freeVars(leftexp) ++ freeVars(rightexp)
    case UnOpExp(_, exp) => freeVars(exp)
    case IfThenElseExp(condexp, thenexp, elseexp) => freeVars(condexp) ++ freeVars(thenexp) ++ freeVars(elseexp)
    case BlockExp(vals, vars, defs, classes, exps) =>
      var fv = Set[Id]()
      for (e2 <- exps)
        fv = fv ++ freeVars(e2)
      for (d <- classes)
        fv = fv ++ freeVars(d)
      for (d <- defs)
        fv = fv ++ freeVars(d)
      for (d <- defs)
        fv = fv -- boundVars(d)
      for (d <- vars.reverse ++ vals.reverse)
        fv = fv -- boundVars(d) ++ freeVars(d)
      fv
    case TupleExp(exps) =>
      var fv = Set[Id]()
      for (exp <- exps)
        fv = fv ++ freeVars(exp)
      fv
    case MatchExp(exp, cases) =>
      var fv = freeVars(exp)
      for (c <- cases)
        fv = fv ++ (freeVars(c.exp) -- c.pattern)
      fv
    case CallExp(funexp, args) =>
      var fv = freeVars(funexp)
      for (a <- args)
        fv = fv ++ freeVars(a)
      fv
    case LambdaExp(params, body) => freeVars(body) -- params.map(p => p.x)
    case AssignmentExp(x, exp) => freeVars(exp) + x
    case WhileExp(guard, body) => freeVars(guard) ++ freeVars(body)
    case NewObjExp(klass, args) =>
      var fv = Set[Id]()
      for (a <- args)
        fv = fv ++ freeVars(a)
      fv
    case LookupExp(objexp, _) => freeVars(objexp)
  }

  def freeVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(_, _, exp) => freeVars(exp)
    case VarDecl(_, _, exp) => freeVars(exp)
    case DefDecl(_, params, _, body) => freeVars(body) -- params.map(p => p.x)
    case ClassDecl(_, params, body) => freeVars(body) -- params.map(p => p.x)
  }

  def boundVars(decl: Decl): Set[Id] = decl match {
    case ValDecl(x, _, _) => Set(x)
    case VarDecl(x, _, _) => Set(x)
    case DefDecl(x, _, _, _) => Set(x)
    case ClassDecl(_, _, _) => Set() // (case not used)
  }

  sealed abstract class Val
  case class IntVal(v: Int) extends Val
  case class BoolVal(v: Boolean) extends Val
  case class FloatVal(v: Float) extends Val
  case class StringVal(v: String) extends Val
  case class TupleVal(vs: List[Val]) extends Val
  case class ClosureVal(params: List[FunParam], restypean: TypeAnnotation, body: Exp, env: Env, cenv: CEnv, defs: List[DefDecl], classes: List[ClassDecl]) extends Val
  case class RefVal(loc: Loc, opttype: Option[Type]) extends Val
  case class ObjectVal(members: Env) extends Val

  case class Constructor(d: ClassDecl, params: List[FunParam], body: BlockExp, env: Env, cenv: CEnv, defs: List[DefDecl], classes: List[ClassDecl])

  type Env = Map[Id, Val]

  type CEnv = Map[Id, Constructor]

  type Sto = Map[Loc, Val]

  type Loc = Int

  def nextLoc(sto: Sto): Loc = sto.size

  def eval(e: Exp, env: Env, cenv: CEnv, sto: Sto): (Val, Sto) = e match {
    case IntLit(c) => (IntVal(c), sto)
    case BoolLit(c) => (BoolVal(c), sto)
    case FloatLit(c) => (FloatVal(c), sto)
    case StringLit(c) => (StringVal(c), sto)
    case NullLit() => (RefVal(-1, None), sto)
    case VarExp(x) =>
      (getValue(env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e)), sto), sto)
    case BinOpExp(leftexp, op, rightexp) =>
      val (leftval, sto1) = eval(leftexp, env, cenv, sto)
      val (rightval, sto2) = eval(rightexp, env, cenv, sto1)
      op match {
        case PlusBinOp =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 + v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (StringVal(v1), StringVal(v2)) => (StringVal(v1 + v2), sto2)
            case (StringVal(v1), IntVal(v2)) => (StringVal(v1 + v2), sto2)
            case (StringVal(v1), FloatVal(v2)) => (StringVal(v1 + v2), sto2)
            case (IntVal(v1), StringVal(v2)) => (StringVal(v1 + v2), sto2)
            case (FloatVal(v1), StringVal(v2)) => (StringVal(v1 + v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '+', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
          }
        case MinusBinOp => (leftval, rightval) match {
          case (IntVal(v1), IntVal(v2)) => (IntVal(v1 - v2), sto2)
          case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto2)
          case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto2)
          case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 - v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at '-', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
        }
        case MultBinOp => (leftval, rightval) match {
          case (IntVal(v1), IntVal(v2)) => (IntVal(v1 * v2), sto2)
          case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto2)
          case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto2)
          case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 * v2), sto2)
          case (StringVal(v1), IntVal(v2)) => (StringVal(v1.*(v2)), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at '*', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
        }
        case DivBinOp =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", e)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 / v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 / v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '/', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
          }
        case ModuloBinOp => if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
          throw new InterpreterError(s"Division by zero", e)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 % v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 % v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '%', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
          }
        case EqualBinOp => (leftval, rightval) match {
          case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 == v2), sto2)
          case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 == v2), sto2)
          case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 == v2), sto2)
          case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 == v2), sto2)
          case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 == v2), sto2)
          case (StringVal(v1), StringVal(v2)) => (BoolVal(v1 == v2), sto2)
          case (TupleVal(v1), TupleVal(v2)) => (BoolVal(v1 == v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at 'Equals', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
        }
        case LessThanBinOp => (leftval, rightval) match {
          case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto2)
          case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto2)
          case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto2)
          case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at 'LessThan', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
        }
        case LessThanOrEqualBinOp => (leftval, rightval) match {
          case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto2)
          case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto2)
          case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto2)
          case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at 'LessThanOrEqual', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
        }
        case MaxBinOp => (leftval, rightval) match {
          case (IntVal(v1), IntVal(v2)) => if (v1 >= v2) (IntVal(v1), sto2) else (IntVal(v2), sto2)
          case (FloatVal(v1), FloatVal(v2)) => if (v1 >= v2) (FloatVal(v1), sto2) else (FloatVal(v2), sto2)
          case (IntVal(v1), FloatVal(v2)) => if (v1 >= v2) (FloatVal(v1), sto2) else (FloatVal(v2), sto2)
          case (FloatVal(v1), IntVal(v2)) => if (v1 >= v2) (FloatVal(v1), sto2) else (FloatVal(v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at 'Max', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
        }
        case AndBinOp => (leftval, rightval) match {
          case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 & v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at '&', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
        }
        case OrBinOp => (leftval, rightval) match {
          case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 | v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at '|', found values ${valueToString(leftval)} and ${valueToString(rightval)}", e)
        }
      }
    case UnOpExp(op, exp) =>
      val (expval, sto1) = eval(exp, env, cenv, sto)
      op match {
        case NegUnOp =>
          expval match {
            case IntVal(v) => (IntVal(-v), sto1)
            case FloatVal(v) => (FloatVal(-v), sto1)
            case _ => throw new InterpreterError(s"Type mismatch at '-', found value ${valueToString(expval)}", exp)
          }
        case NotUnOp => expval match {
          case BoolVal(v) => (BoolVal(!v), sto1)
          case _ => throw new InterpreterError(s"Type mismatch at '!', found value ${valueToString(expval)}", exp)
        }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val condval = eval(condexp, env, cenv, sto)._1
      condval match {
        case BoolVal(true) => eval(thenexp, env, cenv, sto)
        case BoolVal(false) => eval(elseexp, env, cenv, sto)
        case _ => throw new InterpreterError(s"Type mismatch at 'If', found value ${valueToString(condval)}", condexp)
      }
    case b: BlockExp =>
      val (res, _, sto2) = evalBlock(b, env, cenv, sto)
      (res, sto2)
    case TupleExp(exprs) =>
      var (vals, sto2) = (List[Val](), sto)
      for (exp <- exprs) {
        val (v, sto3) = eval(exp, env, cenv, sto2)
        vals = v :: vals
        sto2 = sto3
      }
      (TupleVal(vals.reverse), sto2)
    case MatchExp(mexp, cases) =>
      val (matchval, sto2) = eval(mexp, env, cenv, sto)
      matchval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            if (vs.length == c.pattern.length) {
              val ls = c.pattern.zip(vs)
              val env2 = env ++ ls
              return eval(c.exp, env2, cenv, sto2)
            }
          }
          throw new InterpreterError(s"No case matches value ${valueToString(matchval)}", e)
        case _ => throw new InterpreterError(s"Tuple expected at match, found ${valueToString(matchval)}", e)
      }
    case CallExp(funexp, args) =>
      val (funval, sto2) = eval(funexp, env, cenv, sto)
      funval match {
        case ClosureVal(params, TypeAnnotation(opttype), body, declenv, declcenv, defs, classes) =>
          val (env3, sto3) = evalArgs(args, params, env, sto2, cenv, declenv, e)
          val (env4, cenv4, sto4) = rebindDefsAndClasses(env3, declcenv, sto3, defs, classes)
          val (res, sto5) = eval(body, env4, cenv4, sto4)
          checkValueType(res, opttype, body)
          trace(s"CallExp(Evaluates funexp: $funexp with params: $params")
          (res, sto5)
        case v: Val => throw new InterpreterError(s"Type mismatch at call, expected function, found value ${valueToString(v)}", e)
      }
    case LambdaExp(params, body) => (ClosureVal(params, TypeAnnotation(None), body, env, cenv, List[DefDecl](), List[ClassDecl]()), sto)
    case AssignmentExp(x, exp) =>
      env.getOrElse(x, throw new InterpreterError("Unknown variable!", e)) match {
        case (RefVal(loc, opttype)) =>
          val (newVal, sto1) = eval(exp, env, cenv, sto)
          checkValueType(newVal, opttype, e)
          (TupleVal(List.empty), sto1 + (loc -> newVal))
        case _ => throw new InterpreterError("Not a variable!", e)
      }
    case WhileExp(guard, body) =>
      val (cond, sto1) = eval(guard, env, cenv, sto)
      cond match {
        case BoolVal(true) =>
          val (_, sto2) = eval(body, env, cenv, sto1)
          eval(WhileExp(guard, body), env, cenv, sto2)
        case BoolVal(false) => (TupleVal(List.empty), sto1)
        case _ => throw new InterpreterError("Condition not a boolean!", e)
      }
    case NewObjExp(klass, args) =>
      val constr = cenv.getOrElse(klass, throw new InterpreterError(s"Unknown class name '$klass'", e))
      val (declenv2, declcenv2, _) = rebindDefsAndClasses(constr.env, constr.cenv, sto, constr.defs, constr.classes)
      val (declenv3, sto3) = evalArgs(args, getTypes(constr.d.params, declcenv2, e), env, sto, cenv, declenv2, e)
      val (_, env4, sto4) = evalBlock(constr.body, declenv3, declcenv2, sto3)
      val newloc = nextLoc(sto4)
      val objenv = env4.filterKeys(p => constr.body.defs.exists(d => d.fun == p) || constr.body.vars.exists(d => d.x == p) || constr.body.vals.exists(d => d.x == p))
      val sto5 = sto4 + (newloc -> ObjectVal(objenv))
      (RefVal(newloc, Some(ClassDeclType(constr.d))), sto5)
    case LookupExp(objexp, member) =>
      val (objval, sto2) = eval(objexp, env, cenv, sto)
      objval match {
        case RefVal(loc, opttype) =>
          if(loc == -1) throw new InterpreterError(s"nullpointer exception ", e)
          sto2(loc) match {
            case ObjectVal(members) =>
              trace(s"Evaluates Lookup as ObjectVal: with loc: $loc and members: $member")
              (getValue(members(member), sto2), sto2)
            case v => throw new InterpreterError(s"Base value of lookup is not a reference to an object: $v", e)
          }
        case _ => throw new InterpreterError(s"Base value of lookup is not a location: ${valueToString(objval)}", e)
      }
  }

  /**
    * Evaluates the given block, and returns the resulting value, the updated environment after evaluating all declarations, and the latest store.
    */
  def evalBlock(b: BlockExp, env: Env, cenv: CEnv, sto: Sto): (Val, Env, Sto) = {
    trace(s"evalBlock: $b")
    var envsto = (env, cenv, sto)
    for (d <- b.vals ++ b.vars ++ b.defs ++ b.classes)
      envsto = evalDecl(d, envsto._1, envsto._2, envsto._3, b.defs, b.classes)
    var ressto: (Val, Sto) = (TupleVal(Nil), envsto._3)
    for (exp <- b.exps)
      ressto = eval(exp, envsto._1, envsto._2, ressto._2)
    trace(s"Evaluates the block: $b")
    (ressto._1, envsto._1, ressto._2)
  }

  /**
    * Evaluates the arguments `args` in environment `env` with store `sto`,
    * extends the environment `declenv` with the new bindings, and
    * returns the extended environment and the latest store.
    */
  def evalArgs(args: List[Exp], params: List[FunParam], env: Env, sto: Sto, cenv: CEnv, declenv: Env, e: Exp): (Env, Sto) = {
    if (args.length != params.length) throw new InterpreterError("Wrong number of arguments at call/new", e)
    var (env2, sto2) = (declenv, sto)
    for ((p, arg) <- params.zip(args) ) {
      val (argval, sto3) = eval(arg, env, cenv, sto2)
      checkValueType(argval, p.typean.opttype, arg)
      env2 = env2 + (p.x -> argval)
      sto2 = sto3
    }
    trace(s"Evaluates the arguments: $args in environment: $env with store: $sto")
    (env2, sto2)
  }

  /**
    * Rebinds the defs and classes in the given environment.
    * (The store is unaffected here because we only evaluate 'def' and 'class' declarations.)
    */
  def rebindDefsAndClasses(env: Env, cenv: CEnv, sto: Sto, defs: List[DefDecl], classes: List[ClassDecl]): (Env, CEnv, Sto) = {
    var envsto = (env, cenv, sto)
    for (d <- defs ++ classes) {
      envsto = evalDecl(d, envsto._1, envsto._2, envsto._3, defs, classes)
    }
    trace{s"Rebinds the defs: $defs and the classes: $classes in the given enviroment: $env"}
    envsto
  }

  /**
    * If `v` is a reference to an object or it is a non-reference value, then return `v` itself;
    * otherwise, it must be a reference to a non-object value, so return that value.
    */
  def getValue(v: Val, sto: Sto): Val = v match {
    case RefVal(loc, _) =>
      if(loc == -1) {return v}
      sto(loc) match {
        case _: ObjectVal => trace(s"Evaluates getValue with the type: ObjectVal, the location: $loc value: $v "); v
        case stoval => trace(s"Evaluates getValue with the type: stoval, the location: $loc and value: $stoval "); stoval
      }
    case _ => trace(s"getValue: type: val $v"); v
  }

  def evalDecl(d: Decl, env: Env, cenv: CEnv, sto: Sto, defs: List[DefDecl], classes: List[ClassDecl]): (Env, CEnv, Sto) = d match {
    case ValDecl(x, typean, exp) =>
      val (v, sto1) = eval(exp, env, cenv, sto)
      val ot = getType(typean.opttype, cenv, d)
      checkValueType(v, ot, d)
      (env + (x -> v), cenv, sto1)
    case VarDecl(x, typean, exp) =>
      val (v, sto1) = eval(exp, env, cenv, sto)
      val ot = getType(typean.opttype, cenv, d)
      checkValueType(v, ot, d)
      val loc = nextLoc(sto1)
      (env + (x -> RefVal(loc ,ot)), cenv, sto1 + (loc -> v))
    case DefDecl(fun, params, restypean, body) =>
      val ot = getType(restypean, cenv, d)
      val pot = getTypes(params, cenv, d)
      (env + (fun -> ClosureVal(pot, ot, body, env, cenv, defs, classes)), cenv, sto)
    case d@ ClassDecl(klass, params, body) =>
      (env, cenv + (klass -> Constructor(d, params, body, env, cenv, defs, classes)), sto)
  }

  def getType(ot: Option[Type], cenv: CEnv, n: AstNode): Option[Type] = ot.map(t => {
    def getType(t: Type): Type = t match {
      case ClassType(klass) => ClassDeclType(cenv.getOrElse(klass, throw new InterpreterError(s"Unknown class '$klass'", n)).d);
      case IntType | BoolType | FloatType | StringType => t
      case TupleType(ts) => TupleType(ts.map(tt => getType(tt)))
      case FunType(paramtypes, restype) => FunType(paramtypes.map(tt => getType(tt)), getType(restype))
      case _ => throw new RuntimeException(s"Unexpected type $t") // this case is unreachable...
    }
    getType(t)
  })

  def getType(typean: TypeAnnotation, cenv: CEnv, n: AstNode): TypeAnnotation = TypeAnnotation(getType(typean.opttype, cenv, n))

  def getTypes(params: List[FunParam], cenv: CEnv, n: AstNode): List[FunParam] = params.map(p => FunParam(p.x, getType(p.typean, cenv, p)))

  def checkValueType(v: Val, ot: Option[Type], n: AstNode): Unit = (ot, v) match {
    case (_, RefVal(-1,_))=> //Do nothing
    case (Some(t), _) =>
      (v, t) match {
        case (IntVal(_), IntType) |
             (BoolVal(_), BoolType) |
             (FloatVal(_), FloatType) |
             (IntVal(_), FloatType) |
             (StringVal(_), StringType) => // do nothing
        case (TupleVal(vs), TupleType(ts)) if vs.length == ts.length =>
          for ((vi, ti) <- vs.zip(ts))
            checkValueType(vi, Some(ti), n)
        case (ClosureVal(cparams, TypeAnnotation(optcrestype), _, _, _, _, _), FunType(paramtypes, restype)) if cparams.length == paramtypes.length =>
          for ((p, t) <- cparams.zip(paramtypes))
            checkTypesEqual(p.typean.opttype, t, n)
          checkTypesEqual(optcrestype, restype, n)
        case (RefVal(_, Some(ClassDeclType(vd))), ClassDeclType(td)) =>
          if (vd != td)
            throw new InterpreterError(s"Type mismatch: object of type ${vd.klass} (line ${vd.pos.line} column ${vd.pos.column}) does not match type ${td.klass} (line ${td.pos.line} column ${td.pos.column})", n)
        case _ =>
          throw new InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${Unparser.unparse(t)}", n)
      }
    case (None, _) | (_, RefVal(_, None)) => // do nothing
  }

  def checkTypesEqual(ot1: Option[Type], t2: Type, n: AstNode): Unit = ot1 match {
    case Some(t1) =>
      if (t1 != t2)
        throw new InterpreterError(s"Type mismatch: type ${Unparser.unparse(t1)} does not match expected type ${Unparser.unparse(t2)}", n)
    case None => // do nothing
  }

  def valueToString(v: Val): String = v match {
    case IntVal(c) => c.toString
    case FloatVal(c) => c.toString
    case BoolVal(c) => c.toString
    case StringVal(c) => c
    case TupleVal(vs) => vs.map(v => valueToString(v)).mkString("(", ",", ")")
    case ClosureVal(params, _, exp, _, _, _, _) =>  // the resulting string ignores the result type annotation, the declaration environment, and the sets of defs and classes
      s"<(${params.map(p => p.x + Unparser.unparse(p.typean)).mkString(",")}), ${Unparser.unparse(exp)}>"
    case RefVal(loc, typ) =>
      if(loc == -1) "null"
      else s"#$loc: " + typ // the resulting string ignores the type annotation
    case ObjectVal(_) => "object" // this case is unreachable...
  }

  /**
    * Builds an initial environment, with a value for each free variable in the program.
    */
  def makeInitialEnv(program: Exp): Env = {
    var env = Map[Id, Val]()
    for (x <- freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      env = env + (x -> IntVal(StdIn.readInt()))
    }
    env
  }

  /**
    * Prints message if option -trace is used.
    */
  def trace(msg: String) =
    if (Options.trace)
      println(msg)

  /**
    * Exception thrown in case of MiniScala runtime errors.
    */
  class InterpreterError(msg: String, node: AstNode) extends MiniScalaError(s"Runtime error: $msg", node.pos)
}
