import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DominoTest extends AnyWordSpec with Matchers {
  import DominoSolver._

  def createPieces(rawPieces: Seq[(Int, Int)]): Set[DominoPiece] =
    rawPieces.map { case (a, b) => DominoPiece.create(a, b) }.toSet

  "Classic Domino" should {
    "two pieces failed" in {
      val pieces = createPieces(Seq((0, 0), (1, 1)))
      checkDominoIsSolvable(Mode.Classic, pieces) shouldBe false
    }

    "two pieces success" in {
      val pieces = createPieces(Seq((0, 0), (0, 1)))
      checkDominoIsSolvable(Mode.Classic, pieces) shouldBe true
    }

    "simple check" in {
      val pieces = createPieces(Seq((0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 6)))
      checkDominoIsSolvable(Mode.Classic, pieces) shouldBe true
    }

    "check double piece" in {
      val pieces = createPieces(Seq((0, 0), (0, 1), (0, 2), (0, 3), (0, 4)))
      checkDominoIsSolvable(Mode.Classic, pieces) shouldBe true
      val tooManyPieces = createPieces(Seq((0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5)))
      checkDominoIsSolvable(Mode.Classic, tooManyPieces) shouldBe false
    }

    "full set true" in {
      checkDominoIsSolvable(Mode.Classic, DominoSolver.fullSet) shouldBe true
    }
  }

  "Row Domino" should {
    "two pieces failed" in {
      val pieces = createPieces(Seq((0, 0), (1, 1)))
      checkDominoIsSolvable(Mode.InRow, pieces) shouldBe false
    }

    "two pieces success" in {
      val pieces = createPieces(Seq((0, 0), (0, 1)))
      checkDominoIsSolvable(Mode.InRow, pieces) shouldBe true
    }

    "simple check" in {
      val pieces = createPieces(Seq((0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 6)))
      checkDominoIsSolvable(Mode.InRow, pieces) shouldBe true
    }

    "simple check with doubles" in {
      val pieces = createPieces(
        Seq((0, 1), (1, 1), (1, 2), (2, 2), (2, 3), (3, 3), (3, 4), (4, 4), (4, 5), (5, 5), (5, 6))
      )
      checkDominoIsSolvable(Mode.InRow, pieces) shouldBe true
    }

    "full set true" in {
      checkDominoIsSolvable(Mode.InRow, DominoSolver.fullSet) shouldBe true
    }
  }

  "Ring Domino" should {
    "three pieces ring" in {
      val pieces = createPieces(Seq((0, 1), (1, 2), (2, 0)))
      checkDominoIsSolvable(Mode.Ring, pieces) shouldBe false
    }

    "four pieces ring" in {
      val pieces = createPieces(Seq((0, 1), (1, 2), (2, 3), (3, 0)))
      checkDominoIsSolvable(Mode.Ring, pieces) shouldBe true
    }

    "simple check" in {
      val pieces = createPieces(Seq((0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 6)))
      checkDominoIsSolvable(Mode.Ring, pieces) shouldBe false
    }

    "simple ring check" in {
      val pieces = createPieces(Seq((0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (0, 6)))
      checkDominoIsSolvable(Mode.Ring, pieces) shouldBe true
    }

    "simple ring check with doubles" in {
      val pieces = createPieces(
        Seq((0, 1), (1, 1), (1, 2), (2, 2), (2, 3), (3, 3), (3, 4), (4, 4), (4, 5), (5, 5), (5, 6), (0, 6))
      )
      checkDominoIsSolvable(Mode.Ring, pieces) shouldBe true
    }

    "full set true" in {
      checkDominoIsSolvable(Mode.Ring, DominoSolver.fullSet) shouldBe true
    }
  }
}
