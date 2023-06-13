package patmat

import patmat.Huffman.*

@main def run(): Unit =
  val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
  val t2 = Fork(
    Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5),
    Leaf('d', 4),
    List('a', 'b', 'd'),
    9
  )

  // println(convert(t2))

  val encoded = quickEncode(t2)("ab".toList)
  println(encoded)
  val decoded = decode(t2, encoded)
  println(decoded)
