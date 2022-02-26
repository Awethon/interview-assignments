
import DominoPiece.{CommonPiece, DoublePiece}

import scala.util.Random

sealed trait DominoPiece {
  def value: (Int, Int)
  def contains(i: Int): Boolean = value._1 == i || value._2 == i
}

object DominoPiece {
  def create(a: Int, b: Int): DominoPiece = {
    require(0 <= a && a <= 6)
    require(0 <= b && b <= 6)
    if (a == b) DoublePiece(a, b)
    else if (a > b) CommonPiece(b, a)
    else CommonPiece(a, b)
  }
  case class CommonPiece private(value: (Int, Int)) extends DominoPiece
  case class DoublePiece private(value: (Int, Int)) extends DominoPiece
}

sealed trait Mode
object Mode {
  case object Classic extends Mode
  case object InRow extends Mode
  case object Ring extends Mode
}

object DominoSolver {

  trait Helper {
    def preCheck(pieces: Set[DominoPiece]): Boolean
    def finalCheck(open: Seq[Int]): Boolean
    def extractAvailableValues(piece: DominoPiece): Seq[Int]
    def solve(openValues: Seq[Int],
              availablePieces: Set[DominoPiece]): Boolean = {
      if (availablePieces.isEmpty) {
        finalCheck(openValues)
      } else {
        openValues.exists { o =>
          availablePieces.exists { piece =>
            if (piece.contains(o)) {
              solve(recalculateOpen(o, piece, openValues), availablePieces - piece)
            } else {
              false
            }
          }
        }
      }
    }
    private def recalculateOpen(chosenOpenValue: Int,
                                chosenPiece: DominoPiece,
                                currentOpen: Seq[Int]): Seq[Int] = {
      val openWithoutChosenValue = currentOpen diff Seq(chosenOpenValue)
      val newOpenValues = extractAvailableValues(chosenPiece) diff Seq(chosenOpenValue)
      openWithoutChosenValue ++ newOpenValues
    }
  }

  object ClassicHelper extends Helper {
    def preCheck(pieces: Set[DominoPiece]): Boolean = true
    def finalCheck(open: Seq[Int]): Boolean = true
    def extractAvailableValues(piece: DominoPiece): Seq[Int] =
      piece match {
        case CommonPiece((left, right)) => Seq(left, right)
        case DoublePiece((v, _)) => Seq(v, v, v, v)
      }
  }

  object InRowHelper extends Helper {
    def preCheck(pieces: Set[DominoPiece]): Boolean = true
    def finalCheck(open: Seq[Int]): Boolean = true
    def extractAvailableValues(piece: DominoPiece): Seq[Int] = {
      val (left, right) = piece.value
      Seq(left, right)
    }
  }

  object RingHelper extends Helper {
    def preCheck(pieces: Set[DominoPiece]): Boolean = pieces.size > 3
    def finalCheck(open: Seq[Int]): Boolean = open.size == 2 && open(0) == open(1)
    def extractAvailableValues(piece: DominoPiece): Seq[Int] = {
      val (left, right) = piece.value
      Seq(left, right)
    }
  }

  def chooseHelper(mode: Mode): Helper =
    mode match {
      case Mode.Classic => ClassicHelper
      case Mode.InRow => InRowHelper
      case Mode.Ring => RingHelper
    }

  val fullList: List[DominoPiece] =
    for {
      left <- (0 to 6).toList
      right <- left to 6
    } yield DominoPiece.create(left, right)

  val fullSet: Set[DominoPiece] = fullList.toSet

  def checkDominoIsSolvable(mode: Mode, pieces: Set[DominoPiece]): Boolean = {
    val helper = chooseHelper(mode)

    helper.preCheck(pieces) &&
      helper.solve(
        openValues = helper.extractAvailableValues(pieces.head),
        availablePieces = pieces.tail
      )
  }

}