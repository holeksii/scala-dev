package streams

import Bloxorz.*

class BloxorzSuite extends munit.FunSuite:
  trait SolutionChecker extends GameDef with Solver with StringParserTerrain:
    /** This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves is a
      * valid solution, i.e. leads to the goal.
      */
    import Move.*
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match
          case Left  => block.left
          case Right => block.right
          case Up    => block.up
          case Down  => block.down
      }

  trait Level1 extends SolutionChecker:
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    import Move.*
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)

  test("terrain function level 1 (10pts)") {
    new Level1:
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(4, 11)), "4,11")
  }

  test("find char level 1 (10pts)") {
    new Level1:
      assertEquals(startPos, Pos(1, 1))
  }

  test("block is standing") {
    val x, y = 1
    new Level1:
      val block = Block(Pos(x, y), Pos(x, y))
      assert(Block(Pos(0, 0), Pos(0, 0)).isStanding)
  }

  test("block is in terrain") {
    new Level1:
      val block = Block(Pos(1, 1), Pos(1, 2))
      assert(block.isLegal)
  }

  test("neighbours with history") {
    new Level1:
      val res =
        neighborsWithHistory(
          Block(Pos(1, 1), Pos(1, 1)),
          List(Move.Left, Move.Up)
        )
      import Move.*
      assertEquals(
        res.toSet,
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        )
      )
  }

  test("optimal solution for level 1 (5pts)") {
    new Level1:
      assertEquals(solve(solution), Block(goal, goal))
  }

  test("optimal solution length for level 1 (5pts)") {
    new Level1:
      assertEquals(solution.length, optsolution.length)
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
