package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Your Name>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => if(b==true) 1 else 0
      case S(s) => try s.toDouble catch { case _ => Double.NaN }
      case Undefined => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => if(n==0) false else true
      case S(s) => if(s=="") false else true
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case _ => ???
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case B(b) => B(b)
      case N(n) => N(n)
      case S(s) => S(s)
      case Undefined => Undefined
      /* Inductive Cases */
      case Binary(bop, e1, e2) => bop match {

        case Plus => N(toNumber(eval(e1)) + toNumber(eval(e2)))
        case Minus => N(toNumber(eval(e1)) - toNumber(eval(e2)))
        case Times => N(toNumber(eval(e1)) * toNumber(eval(e2)))
        case Div => N(toNumber(eval(e1)) / toNumber(eval(e2)))

        case Or => if(toBoolean(eval(e1))==true) e1 else e2
        case And => if(toBoolean(eval(e1))==false) e1 else e2

        case Eq => if(e1==e2) B(true) else B(false)
        case Ne => if(e1==e2) B(false) else B(true)
        case Lt => if(toNumber(eval(e1)) < toNumber(eval(e2))) B(true) else B(false)
        case Le => if(toNumber(eval(e1)) <= toNumber(eval(e2))) B(true) else B(false)
        case Gt => if(toNumber(eval(e1)) > toNumber(eval(e2))) B(true) else B(false)
        case Ge => if(toNumber(eval(e1)) >= toNumber(eval(e2))) B(true) else B(false)

        case Seq => eval(e2)
      }
      case Unary(uop, e1) => uop match{
        case Neg => N(-toNumber(eval(e1)))
        case Not => B(!toBoolean(eval(e1)))
      }
      case If(e1, e2, e3) => if(e1==B(true)) eval(e2) else eval(e3)




      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case _ => ???
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}

