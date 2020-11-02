package miniscala

import scala.collection.mutable
import scala.io.StdIn

object AbstractMachine {

  case class Executable(freevars: List[String], code: List[Instruction])

  sealed abstract class Instruction

  case class Const(c: Int) extends Instruction

  case object Unit extends Instruction

  case object Add extends Instruction

  case object Sub extends Instruction

  case object Mul extends Instruction

  case object Div extends Instruction

  case object Eq extends Instruction

  case object Lt extends Instruction

  case object Leq extends Instruction

  case object And extends Instruction

  case object Or extends Instruction

  case object Neg extends Instruction

  case object Not extends Instruction

  case object Dup extends Instruction

  case object Pop extends Instruction

  case class Branch(thencode: List[Instruction], elsecode: List[Instruction]) extends Instruction

  case class Loop(condcode: List[Instruction], bodycode: List[Instruction]) extends Instruction

  case object Enter extends Instruction

  case class EnterDefs(num: Int) extends Instruction

  case class Exit(num: Int) extends Instruction

  case class Read(index: IdIndex) extends Instruction

  case object Alloc extends Instruction

  case object Load extends Instruction

  case object Store extends Instruction

  case class Lambda(freeids: List[IdIndex], body: List[Instruction]) extends Instruction

  case class Call(arity: Int, tailcall: Boolean) extends Instruction

  case object Return extends Instruction

  type IdIndex = Int // index of identifier in envstack

  def execute(program: Executable, initialEnv: List[Int]): Int = {

    sealed abstract class Value
    case class IntVal(c: Int) extends Value
    case class RefVal(loc: Loc) extends Value
    class ClosureVal(val body: List[Instruction], var env: List[Value]) extends Value {
      override def toString: String = s"Code($body)" // omitting env in toString to avoid infinite recursion
    }

    type Loc = Int // location in store

    class Frame {
      var code: List[Instruction] = Nil // the program code to be executed
      val opstack = new mutable.ArrayStack[Value] // operand stack, contains values of sub-expressions
      val envstack = new mutable.ArrayStack[Value] // environment stack, contains values of identifiers
    }

    var frame = new Frame // the current frame
    frame.code = program.code
    initialEnv.foreach(c => frame.envstack.push(IntVal(c)))

    val callstack = new mutable.ArrayStack[Frame] // call stack (excluding the current frame)
    val store = new mutable.ArrayBuffer[Value] // store, maps locations to values

    def popInt() = frame.opstack.pop().asInstanceOf[IntVal].c

    def popLoc() = frame.opstack.pop().asInstanceOf[RefVal].loc

    def popClosure() = frame.opstack.pop().asInstanceOf[ClosureVal]

    try {
      while (frame.code.nonEmpty) {
        val inst = frame.code.head
        frame.code = frame.code.tail
        trace(s"Current operand stack:     ${frame.opstack.mkString("[", ", ", "]")}")
        trace(s"Current environment stack: ${frame.envstack.mkString("[", ", ", "]")}")
        trace(s"Call stack:                ${callstack.map(f => s"(${f.code.mkString("[", ", ", "]")}, ${f.opstack.mkString("[", ", ", "]")}, ${f.envstack.mkString("[", ", ", "]")})").mkString("[", ", ", "]")}")
        trace(s"Store:                     ${store.mkString("[", ", ", "]")}")
        trace(s"Next instruction:          $inst")
        inst match {
          case Const(c) =>
            frame.opstack.push(IntVal(c))
          case Unit =>
            frame.opstack.push(IntVal(0)) // just represent unit as '0'
          case Add =>
            val c2 = popInt()
            val c1 = popInt()
            frame.opstack.push(IntVal(c1 + c2))
          case Sub =>
            val c2 = popInt()
            val c1 = popInt()
            frame.opstack.push(IntVal(c1 - c2))
          case Mul =>
            val c2 = popInt()
            val c1 = popInt()
            frame.opstack.push(IntVal(c1 * c2))
          case Div =>
            val c2 = popInt()
            val c1 = popInt()
            frame.opstack.push(IntVal(c1 / c2))
          case Eq =>
            val c2 = popInt()
            val c1 = popInt()
            if (c1 == c2) frame.opstack.push(IntVal(1)) else frame.opstack.push(IntVal(0))
          case Lt =>
            val c2 = popInt()
            val c1 = popInt()
            if (c1 < c2) frame.opstack.push(IntVal(1)) else frame.opstack.push(IntVal(0))
          case Leq =>
            val c2 = popInt()
            val c1 = popInt()
            if (c1 <= c2) frame.opstack.push(IntVal(1)) else frame.opstack.push(IntVal(0))
          case And =>
            val c2 = popInt()
            val c1 = popInt()
            if (c1 == 1 && c2 == 1) frame.opstack.push(IntVal(1)) else frame.opstack.push(IntVal(0))
          case Or =>
            val c2 = popInt()
            val c1 = popInt()
            if (c1 == 1 || c2 == 1) frame.opstack.push(IntVal(1)) else frame.opstack.push(IntVal(0))
          case Neg =>
            val c = popInt()
            frame.opstack.push(IntVal(-c))
          case Not =>
            val c = frame.opstack.pop()
            if (c == IntVal(0)) frame.opstack.push(IntVal(1)) else frame.opstack.push(IntVal(0))
          case Dup =>
            frame.opstack.dup()
          case Pop =>
            frame.opstack.pop()
          case Branch(thencode, elsecode) =>
            if (popInt() == 1)
              frame.code = thencode ++ frame.code
            else
              frame.code = elsecode ++ frame.code
          case Loop(condcode, bodycode) =>
            frame.code = condcode ++ (Branch(bodycode ++ List(Loop(condcode, bodycode)), Nil) :: frame.code)
          case Enter =>
            frame.envstack.push(frame.opstack.pop())
          case Exit(num) =>
            for (i <- 1 to num)
              frame.envstack.pop()
          case Read(index) =>
            frame.opstack.push(frame.envstack(index))
          case Alloc =>
            frame.opstack.push(RefVal(store.size))
            store += IntVal(0)
          case Load =>
            val loc = popLoc()
            frame.opstack.push(store(loc))
          case Store =>
            val v = frame.opstack.pop()
            val loc = popLoc()
            store(loc) = v
          case Lambda(freeids, body) =>
            val env = freeids.map(x => frame.envstack(x)) // creates env for free identifiers (excluding defs in same block)
            frame.opstack.push(new ClosureVal(body, env))
          case EnterDefs(num) =>
            val cls = (1 to num).map(_ => popClosure()).reverse
            cls.foreach(cl => {
              cl.env = cl.env ++ cls.toList // adds all the closures to the env of each closure
              frame.envstack.push(cl)
            })
          case Call(arity, tailcall) =>
            val newframe = new Frame
            for (i <- 1 to arity) // passes the values of the parameters
              newframe.envstack.push(frame.opstack.pop())
            val cl = popClosure()
            for (v <- cl.env.reverse) // copies the values of the free identifiers (excluding defs in same block) followed by all the defs
              newframe.envstack.push(v)
            newframe.code = cl.body
            callstack.push(frame)
            frame = newframe
          case Return =>
            if (callstack.nonEmpty) {
              val v = frame.opstack.pop()
              frame = callstack.pop()
              frame.opstack.push(v)
            } else
              frame.code = Nil // signal to terminate the execution loop
        }
      }
      popInt()
    } catch {
      case ex: Exception => throw new AbstractMachineError(ex)
    }
  }

  def valid(prog: List[Instruction], stack: (Int, Int)): Boolean = {
    def validate(program: List[Instruction], stack: (Int, Int)): (Int, Int) = {
      if (program.isEmpty) stack
      else {
        program.head match {
          case Const(_) =>
            validate(program.tail, (stack._1 + 1, stack._2))
          case Unit =>
            validate(program.tail, (stack._1 + 1, stack._2))
          case Add | Sub | Mul | Div | Eq | Lt | Leq | And | Or =>
            if (stack._1 < 2) throw new AbstractMachineError(new Exception) else validate(program.tail, (stack._1 - 1, stack._2))
          case Neg | Not =>
            if (stack._1 == 0) throw new AbstractMachineError(new Exception) else validate(program.tail, stack)
          case Pop =>
            if (stack._1 == 0) throw new AbstractMachineError(new Exception) else validate(program.tail, (stack._1 - 1, stack._2))
          case Dup =>
            if (stack._1 == 0) throw new AbstractMachineError(new Exception) else validate(program.tail, (stack._1 + 1, stack._2))
          case Branch(thencode, elsecode) =>
            if (validate(thencode, stack)._1 == validate(elsecode, stack)._1) {
              if (stack._1 == 0) throw new AbstractMachineError(new Exception) else validate(program.tail, (stack._1 - 1, stack._2))
            }
            else throw new AbstractMachineError(new Exception)
          case Loop(condcode, bodycode) =>
            val cond = validate(condcode, stack)
            val body = validate(bodycode, stack)
            if (cond == (stack._1 + 1, stack._2) && body == stack) validate(program.tail, stack) else throw new AbstractMachineError(new Exception)
          case Enter =>
            if (stack._1 == 0) throw new AbstractMachineError(new Exception) else validate(program.tail, (stack._1 - 1, stack._2 + 1))
          case Exit(num) =>
            if (stack._2 < num) throw new AbstractMachineError(new Exception) else validate(program.tail, (stack._1, stack._2 - num))
          case Read(index) =>
            if (stack._2 < index) throw new AbstractMachineError(new Exception) else validate(program.tail, (stack._1 + 1, stack._2))
          case Alloc =>
            validate(program.tail, (stack._1 + 1, stack._2))
          case Load =>
            if (stack._1 == 0) throw new AbstractMachineError(new Exception) else validate(program.tail, stack)
          case Store =>
            if (stack._1 < 2) throw new AbstractMachineError(new Exception) else validate(program.tail, (stack._1 - 2, stack._2))
          case EnterDefs(num) =>
            if (stack._1 < num) throw new AbstractMachineError(new Exception) else validate(program.tail, (stack._1 - num, stack._2 + num))
        }
      }
    }
    validate(prog, stack)
    true
  }

  def makeInitialEnv(program: Executable): List[Int] = {
    program.freevars.foldLeft(List[Int]())((env, x) => {
      print(s"Please provide an integer value for the variable $x: ")
      StdIn.readInt() :: env
    })
  }

  def trace(msg: => String): Unit = if (Options.trace) println(msg)

  class AbstractMachineError(ex: Exception) extends Exception(ex)

}
