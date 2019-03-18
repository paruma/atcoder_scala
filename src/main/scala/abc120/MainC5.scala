package abc120

import java.util.Scanner


// Stackを用いた解
object MainC5 {
  def read() = {
    val sc = new Scanner(System.in)
    val cubes = sc.next()
    cubes
  }

  // Stackは非推奨。Listを使うことが推奨されている。
  def step(stack: List[Char], cube: Char): List[Char] = {
    if(stack.isEmpty){
      List(cube)
    }else{
      val stackTop = stack.head
      if(stackTop != cube){
        stack.tail
      }else{
        cube::stack
      }
    }
  }


  def solve(cubes: String): Int = {
    val stack = cubes.foldLeft[List[Char]](List.empty)(step)
    cubes.length - stack.length
  }


  def main(args: Array[String]): Unit = {
    val cubes = read()
    println(solve(cubes))
  }
}