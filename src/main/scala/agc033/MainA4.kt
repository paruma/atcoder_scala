package agc033

import java.util.*

fun read(): Triple<Int, Int, Array<String>> {
    val sc = Scanner(System.`in`)
    val h = sc.nextInt()
    val w = sc.nextInt()
    val a = Array<String>(h) { sc.next() }
    return Triple(w, h, a)
}

fun read2(): Triple<Int, Int, Array<String>> {
    val (h, w) = readLine()!!.split(" ").map(String::toInt)
    val a = Array(h){readLine()!!}
    return Triple (w, h, a)
}


fun solve(w: Int, h: Int, a: Array<String>): Int {


    data class Point(val x: Int, val y: Int)

    fun isWithin(p: Point): Boolean {
        return (p.x in 0 until w) && (p.y in 0 until h)
    }

    val open: Queue<Point> = ArrayDeque()
    val infty = w + h
    val dist = Array(h) { Array(w) { infty } }
    val visited = Array(h) { Array(w) { false } }
    for (y in 0 until h) {
        for (x in 0 until w) {
            if (a[y][x] == '#') {
                dist[y][x] = 0
                visited[y][x] = true
                open.add(Point(x, y))
            }
        }
    }

    var maxDist = 0
    fun update(current: Point, next: Point) {
        if (isWithin(next) && !visited[next.y][next.x]) {
            open.add(next)
            visited[next.y][next.x] = true
            dist[next.y][next.x] = dist[current.y][current.x] + 1
            maxDist = Math.max(maxDist, dist[next.y][next.x])
        }
    }
    while (open.isNotEmpty()) {
        val current = open.remove()
        update(current, Point(current.x + 1, current.y))
        update(current, Point(current.x - 1, current.y))
        update(current, Point(current.x, current.y + 1))
        update(current, Point(current.x, current.y - 1))
    }
    return maxDist
}

fun main(args: Array<String>) {
    val (w, h, a) = read2()
    println(solve(w, h, a))
}
