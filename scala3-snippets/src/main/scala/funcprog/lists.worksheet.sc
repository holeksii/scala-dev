def flatten(xs: Any): List[Any] = xs match
    case Nil => Nil
    case y :: ys => flatten(y) ::: flatten(ys)
    case _ => xs :: Nil


val exampleList = 0 :: List(List(1, 2, List(3, 4), 5, List(6, 7, 8, 9)))
flatten(exampleList)
