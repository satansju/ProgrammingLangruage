package miniscala

import miniscala.Interpreter._
import miniscala.parser.Parser.parse

object Test132 {

  def main(args: Array[String]): Unit = {
    testValFail("""{ var z = null; z.f }""")
    testVal("""{ class C() { }; { var x: C = null; x = new C() } }""".stripMargin, TupleVal(List[Val]()))
    testVal("""{ class C() { }; { var x: C = new C(); x = null } }""".stripMargin, TupleVal(List[Val]()))
    // <-- add more test cases here
  }

  def testVal(prg: String, value: Val, env: Env = Map(), cenv: CEnv = Map(), sto: Sto = Map()) = {
    val (res, _) = eval(parse(prg), env, cenv, sto)
    assert(res == value)
  }

  def testValFail(prg: String, env: Env = Map(), cenv: CEnv = Map(), sto: Sto = Map()) = {
    try {
      eval(parse(prg), env, cenv, sto)
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

}