package sandbox


fun <A, B> Iterable<A>.scan(z: B, op: (B, A) -> B): MutableList<B> {
    var acc = z
    val accList = mutableListOf(z)
    for (x in this) {
        acc = op(acc, x)
        accList.add(acc)
    }
    return accList
}

fun Iterable<Long>.cumSum(): MutableList<Long> {
    return this.scan(0L) { x, y -> x + y }
}


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

fun fracGF(n: Int): GF = (1..n).map { GF(it.toLong()) }.fold(GF(1)) { x, y -> x * y }

fun combGF(n: Int, r: Int): GF {
    if (r < 0) return GF(0)
    if (n < r) return GF(0)
    return fracGF(n) / (fracGF(r) * fracGF(n - r))
}

val cumFracGF = (1..10).map { GF(it.toLong()) }.scan(GF(1)) { x, y -> x * y }


tailrec fun gcd(x: Long, y: Long): Long {
    // x,y>=0
    return if (y == 0L) x else gcd(y, x % y)
}

fun divisor(n: Long): MutableList<Long> {
    val result = mutableListOf<Long>()
    for (i in 1..Math.sqrt(n.toDouble()).toInt()) {
        if (n % i == 0L) {
            result.add(i.toLong())
            if (i.toLong() * i.toLong() != n) {
                result.add(n / i)
            }
        }
    }
    result.sort()
    return result
}

fun divCeil(a: Long, b: Long): Long = (a - 1) / b + 1


data class RunLengthElem(val nRepeats: Int, val elem: Char)

fun compressRunLength(s: String): MutableList<RunLengthElem> {
    var currentChar = s[0]
    var currentCount = 1
    val result = arrayListOf<RunLengthElem>()
    for (i in 1 until s.length) {
        if (currentChar == s[i]) {
            currentCount++
        } else {
            result.add(RunLengthElem(currentCount, currentChar))
            currentChar = s[i]
            currentCount = 1
        }
    }
    result.add(RunLengthElem(currentCount, currentChar))
    return result
}

class UnionFind(val n: Int) {
    sealed class Node {
        class Root(val count: Int) : Node()
        class NonRoot(val parent: Int) : Node()
    }

    private val nodes = Array<Node>(n) { Node.Root(1) }

    data class RootInfo(val idx: Int, val body: Node.Root)

    private fun root(x: Int): RootInfo {
        return when (val node = nodes[x]) {
            is Node.Root -> RootInfo(x, node)
            is Node.NonRoot -> {
                val rootInfo = root(node.parent)
                nodes[x] = Node.NonRoot(rootInfo.idx)
                rootInfo
            }
        }
    }

    fun same(x: Int, y: Int): Boolean {
        return root(x).idx == root(y).idx
    }

    fun unite(x: Int, y: Int) {
        val rx = root(x)
        val ry = root(y)
        if (rx.idx == ry.idx) return

        // rxをryにつける
        nodes[rx.idx] = Node.NonRoot(ry.idx)
        nodes[ry.idx] = Node.Root(rx.body.count + ry.body.count)
    }

    fun sameCount(x: Int): Int {
        return root(x).body.count
    }


}

fun main() {
    println(Array(10) { it.toLong() }.asIterable().cumSum().joinToString(" "))
    println(cumFracGF.joinToString(" "))
    println(combGF(6, 3))
    println(gcd(128, 96))
    println(gcd(96, 128))
    println(divisor(12).joinToString(" "))
    println(divisor(36).joinToString(" "))
    println(divCeil(7, 4))
    println(divCeil(8, 4))
    println(compressRunLength("AAABCDDC"))

    val f = {
        println("union find")
        val uf = UnionFind(5)
        println(uf.same(0, 1))
        println(uf.sameCount(0))

        uf.unite(0, 1)
        println(uf.same(0, 1))
        println(uf.sameCount(0))

    }
    f()
}