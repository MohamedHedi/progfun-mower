package com.mowitnow.mower
import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile
import scala.util.parsing.combinator.JavaTokenParsers

object Main /*extends App*/ {
  //coordinates
  type Position = (Int, Int)
  //Mower : Position + orientation
  case class Mower(position: Position, orientation: Char) {
    override def toString = "((" + position._1.toString + ", " + position._2.toString + "), " + orientation.toString + ")"
  }

  //Mower controller
  case class Controller(maxPosition: Position) {
    //execute instruction recursively
    def instruct(initMower: Mower, instructions: String): Mower = {
      instructions.foldLeft(initMower)((mower, command) => this.process(mower, command))
    }

    //process command
    def process(that: Mower, command: Char): Mower = (that, command) match {
      //go right command
      case (Mower(c, 'N'), 'D') => Mower(c, 'E')
      case (Mower(c, 'E'), 'D') => Mower(c, 'S')
      case (Mower(c, 'S'), 'D') => Mower(c, 'W')
      case (Mower(c, 'W'), 'D') => Mower(c, 'N')
      //go left command
      case (Mower(c, 'N'), 'G') => Mower(c, 'W')
      case (Mower(c, 'W'), 'G') => Mower(c, 'S')
      case (Mower(c, 'S'), 'G') => Mower(c, 'E')
      case (Mower(c, 'E'), 'G') => Mower(c, 'N')
      //forward command
      case (Mower((x, y), 'N'), 'A') => if (y + 1 > maxPosition._2) that else Mower((x, y + 1), 'N')
      case (Mower((x, y), 'S'), 'A') => if (y - 1 < 0) that else Mower((x, y - 1), 'S')
      case (Mower((x, y), 'E'), 'A') => if (x + 1 > maxPosition._1) that else Mower((x + 1, y), 'E')
      case (Mower((x, y), 'W'), 'A') => if (x - 1 < 0) that else Mower((x - 1, y), 'W')
      //other
      case (m, _) => m
    }
  }

  //Parser des commandes fourni sous forme de string
  import scala.util.parsing.combinator._
  //Type l'erreur de parsing
  case class InvalidInputException(msg: String) extends Exception(msg)

  object InputParser extends JavaTokenParsers {
    //parsing limits
    val limitsParser: Parser[Position] = "\\d{1,}".r ~ "\\d{1,}".r ^^ { case x ~ y => (x.toInt, y.toInt) }
    def limits(line: String): Position = {
      parseAll(limitsParser, line) match {
        case Success(r, _) => r
        case e: NoSuccess => throw InvalidInputException(e.msg)
      }
    }

    //Parse initial mower
    val stateParser: Parser[Mower] = "\\d{1,}".r ~ "\\d{1,}".r ~ "[NESW]".r ^^ { case x ~ y ~ o => Mower((x.toInt, y.toInt), o.charAt(0)) }
    def state(line: String): Mower = {
      parseAll(stateParser, line) match {
        case Success(r, _) => r
        case e: NoSuccess => throw InvalidInputException(e.msg)
      }
    }

    //parse instruction
    val instructionsParser: Parser[String] = "[DGA]+".r
    def instructions(line: String): String = {
      parse(instructionsParser, line) match {
        case Success(r, _) => r
        case e: NoSuccess => throw InvalidInputException(e.msg)
      }
    }
  }

  object Program {
    import InputParser._
    def solveIterator(iter: Iterator[String]): List[Mower] = {

      val controller = if (iter.hasNext) Controller(limits(iter.next))
      else throw InvalidInputException("Invalid file structure : unable to read limits")

      def actions(controller: Controller, group: iter.GroupedIterator[String]): ListBuffer[Mower] = {
        def foldIter(e: Seq[String], acc: ListBuffer[Mower]): ListBuffer[Mower] = e match {
          case List(s, i) => controller.instruct(state(s), instructions(i)) +=: acc
          case _ => throw InvalidInputException("Invalid file structure : unable to read mower position and instructions")
        }
        group.foldRight(ListBuffer[Mower]())(foldIter _)
      }

      actions(controller, iter grouped 2).toList
    }

    //reading from file
    def solveFile(filePath: String): List[Mower] = {
      val source = fromFile(filePath)
      val result = solveIterator(source.getLines)
      source.close()
      result
    }
  }
  def main(args: Array[String]): Unit = {
    Program.solveFile("instructions.txt").foreach(println)
  }
}