package abc120

import java.util.Scanner

// TLE解
object MainC {
  def read() = {
    val sc = new Scanner(System.in)
    val cubes = sc.next()
    cubes
  }

  def solve(cubes: String) :Int ={
    var cubesTest = cubes
    while(true){
      val beforeLen = cubesTest.length
      cubesTest = cubesTest.replace("01", "")
      cubesTest = cubesTest.replace("10", "")
      val afterLen = cubesTest.length
      if(beforeLen == afterLen) return cubes.length - afterLen
    }
    -1
  }


  def main(args: Array[String]): Unit = {
    val cubes = read()
    println(solve(cubes))
  }
}