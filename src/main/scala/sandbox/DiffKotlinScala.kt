package sandbox

import java.util.*

fun read(): Pair<Int, Int> {
    val sc = Scanner(System.`in`)
    // val a,b = sc.nextInt()みたいに同時代入はできない
    val a = sc.nextInt()
    val b = sc.nextInt()
    return Pair(a, b)
}

fun solve(a: Int, b: Int): Int {
    return a + b
}

fun main() {
    val (a, b) = read()
    println(solve(a, b))
}