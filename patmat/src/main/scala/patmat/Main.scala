package patmat

@main def run(): Unit =
  ???


def sum(xs: List[Int]): Int =
  xs match
    case Nil => 0
    case x :: xs1 => x + sum(xs1)
    