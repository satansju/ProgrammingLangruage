package miniscala

/**
  * Handles the command-line options and the name of the MiniScala source file.
  */
object Options {

  val usage =
    """
      |Usage: miniscala [options] <filename>
      |
      |Options:
      |
      |  (none)    only parse the program
      |  -unparse  parse and unparse the program
      |  -run      parse and run the program
      |  -types    parse and type-check the program
      |  -trace    trace when running
      |  -compile  parse and compile to abstract machine code
      |  -machine  load and run abstract machine code
    """.stripMargin

  var unparse = false

  var run = false

  var types = false

  var trace = false

  var compile = false

  var machine = false

  var file: String = _

  /**
    * Parses the command-line arguments.
    */
  def parse(args: Array[String]) = {
    for (arg <- args) {
      if (arg.startsWith("-")) {
        arg match {
          case "-unparse" => unparse = true
          case "-run" => run = true
          case "-types" => types = true
          case "-trace" => trace = true
          case "-compile" => compile = true
          case "-machine" => machine = true
          case _ =>
            throw new OptionsError(s"Error: option not recognized: $arg")
        }
      } else {
        if (file != null)
          throw new OptionsError("Error: cannot process more than one file")
        file = arg
      }
    }
    if (file == null)
      throw new OptionsError("Error: file name missing")
  }

  /**
    * Exception thrown in case of illegal command-line arguments.
    */
  class OptionsError(msg: String) extends Exception(msg)
}
