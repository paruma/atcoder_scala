package abc133

import java.util.*


data class Edge(val a: Int, val b: Int)

data class GF(val x: Long) {
    companion object {
        const val MOD: Long = (1e9 + 7).toLong()

        fun powGF(x: GF, t: Long): GF {
            if (t == 0L) return GF(1)
            val y = powGF(x, t / 2L)
            return if (t % 2L == 0L) {
                y * y
            } else {
                x * y * y
            }
        }
    }

    operator fun plus(that: GF): GF {
        return GF((this.x + that.x) % MOD)
    }

    operator fun minus(that: GF): GF {
        return GF((this.x - that.x + MOD) % MOD)
    }

    operator fun times(that: GF): GF {
        return GF((this.x * that.x) % MOD)
    }

    operator fun unaryMinus(): GF {
        return GF((-this.x + MOD) % MOD)
    }

    fun inv(): GF {
        return powGF(this, MOD - 2)
    }

    operator fun div(that: GF): GF {
        return this * that.inv()
    }
}

fun main(args: Array<String>) {
    val (n, k) = readLine()!!.split(" ").map(String::toInt)
    val edges = Array(n - 1) {
        val (a, b) = readLine()!!.split(" ").map(String::toInt)
        Edge(a - 1, b - 1)
    }

    val nextList = Array(n) {
        mutableListOf<Int>()
    }
    for (edge in edges) {
        nextList[edge.a].add(edge.b)
        nextList[edge.b].add(edge.a)
    }

    val queue: Queue<Int> = ArrayDeque()
    val visited = Array(n) { false }

    val numNextVisited = Array(n) { 0 }
    val score = Array(n) { -123 }

    queue.add(0)
    visited[0] = true
    score[0] = k

    while (queue.isNotEmpty()) {
        val current = queue.remove()
        for (next in nextList[current]) {
            if (!visited[next]) {
                queue.add(next)
                visited[next] = true

                score[next] = k - (1 + numNextVisited[current])
                numNextVisited[next]++
                numNextVisited[current]++
            }
        }
    }
    val result = score.map { GF(it.toLong()) }.fold(GF(1)) { x, y -> x * y }.x
    println(result)

}