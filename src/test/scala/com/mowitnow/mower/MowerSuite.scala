package com.mowitnow.mower
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import com.mowitnow.mower.Main.Mower
import com.mowitnow.mower.Main.Program.solveFile
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class MowerSuite extends FunSuite {
  test("given instructions") {
    solveFile("instructions.txt").foreach(println)
    assert(solveFile("instructions.txt") === List(Mower((1, 3), 'N'), Mower((5, 1), 'E')))
  }
  
    test("test max position") {
    solveFile("instructions.txt").foreach(println)
    assert(solveFile("instructions2.txt") === List(Mower((1, 3), 'S')))
  }
}
