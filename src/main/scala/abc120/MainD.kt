package abc120


data class Edge(val a: Int, val b: Int)
class UnionFind(val n: Int) {
    sealed class Node {
        class Root(val count: Int) : Node()
        class NonRoot(val parent: Int) : Node()
    }

    private val nodes = Array<Node>(n) { Node.Root(1) }

    data class RootInfo(val idx: Int, val body: Node.Root)

    private fun root(x: Int): RootInfo {
        val node = nodes[x]
        return when (node) {
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


fun main(args: Array<String>) {
    val (nVertex, nEdge) = readLine()!!.split(" ").map(String::toInt)
    val edges = Array(nEdge) {
        val (a, b) = readLine()!!.split(" ").map(String::toInt)
        Edge(a - 1, b - 1)
    }

    val uf = UnionFind(nVertex)
    edges.reverse()

    val diffList = edges.map {
        if (uf.same(it.a, it.b)) {
            0L
        } else {
            val diff = uf.sameCount(it.a).toLong() * uf.sameCount(it.b).toLong()
            uf.unite(it.a, it.b)
            diff
        }
    }

    val allCount = nVertex.toLong() * (nVertex - 1) / 2
    val result = diffList.cumSum().take(nEdge).map { allCount - it }.reversed()

    result.forEach { println(it) }
}