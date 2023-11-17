package parallelprog

enum Tree[T]:
  case Leaf(a: Array[T])
  case Node(left: Tree[T], right: Tree[T])

  def mapPar[R: Manifest](f: T => R): Tree[R] =
    this match
      case Leaf(a) =>
        val res = new Array[R](a.length)
        mapArrayPar(a, f, 1, res)
        Leaf(res)
      case Node(l, r) =>
        val (lRes, rRes) = parallel(l.mapPar(f), r.mapPar(f))
        Node(lRes, rRes)
