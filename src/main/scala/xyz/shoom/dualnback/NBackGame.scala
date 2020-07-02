package xyz.shoom.dualnback

import cats.data._
import cats.implicits._

import scala.util.Random

object NBackGame {
  final case class Stimulus(visual: Int, audio: Char)

  sealed trait MatchAttempt
  final case class VisualMatchAttempt(idx: Int) extends MatchAttempt
  final case class AudioMatchAttempt(idx: Int) extends MatchAttempt

  val possibleVisuals: Vector[Int] = (0 to 8).toVector.filterNot(_ == 4) // position going from left to right, top down
  val possibleAudio: Vector[Char] = "cghkpqtw".toVector

  def shuffleSeq[A](xs: Seq[A]): State[Random, Seq[A]] = State(r => (r, r.shuffle(xs)))

  // TODO: consider parameterizing the number of visual, audio, and dual targets
  def generateTargetPositions(numTrials: Int, nBackLevel: Int): State[Random, (Set[Int], Set[Int], Set[Int])] = {
    for {
      shuffledPositions <- shuffleSeq(nBackLevel until numTrials)
      visualTargetPositions = shuffledPositions.slice(0, 4).toSet
      audioTargetPositions = shuffledPositions.slice(4, 8).toSet
      dualTargetPositions = shuffledPositions.slice(8, 10).toSet
    } yield (visualTargetPositions, audioTargetPositions, dualTargetPositions)
  }

  /**
    * Generate signals with targets placed at indexes in the (visual, audio, dual) tuple.
    */
  def generateSignalsAndTargets(numTrials: Int, nBackLevel: Int): State[Random, (Seq[Stimulus], (Set[Int], Set[Int], Set[Int]))] = {
    // TODO: use the State's random
    val rand = new Random()
    def randElemFromArray[A](xs: Vector[A]): A = xs(rand.nextInt(xs.length))
    def generateSignal(visuals: Vector[Int], audio: Vector[Char]): Stimulus =
      Stimulus(randElemFromArray(visuals), randElemFromArray(audio))

    val targetPositions = generateTargetPositions(numTrials, nBackLevel)
    targetPositions.map { tPos =>
      val (visPos, audioPos, dualPos) = tPos
      (0 until numTrials).foldLeft(Vector.empty[Stimulus]) { (acc: Vector[Stimulus], i: Int) => acc :+
        (
          if (i < nBackLevel) generateSignal(possibleVisuals, possibleAudio)
          else if (visPos.contains(i)) generateSignal(Vector(acc.reverse(nBackLevel-1).visual), possibleAudio.filterNot(_ == acc.reverse(nBackLevel-1).audio))
          else if (audioPos.contains(i)) generateSignal(possibleVisuals.filterNot(_ == acc.reverse(nBackLevel-1).visual), Vector(acc.reverse(nBackLevel-1).audio))
          else if (dualPos.contains(i)) generateSignal(Vector(acc.reverse(nBackLevel-1).visual), Vector(acc.reverse(nBackLevel-1).audio))
          else generateSignal(possibleVisuals.filterNot(_ == acc.reverse(nBackLevel-1).visual), possibleAudio.filterNot(_ == acc.reverse(nBackLevel-1).audio))
        )
      } -> tPos
    }
  }

  def countMistakes(
    visualTargetPositions: Set[Int],
    audioTargetPositions: Set[Int],
    dualTargetPositions: Set[Int],
    matchAttempts: Set[MatchAttempt]
  ): (Int, Int) = {
    val visualIndexesGuessed = matchAttempts.collect{case VisualMatchAttempt(idx) => idx}
    val audioIndexesGuessed = matchAttempts.collect{case AudioMatchAttempt(idx) => idx}
    (
      symmetricDifference(visualTargetPositions ++ dualTargetPositions, visualIndexesGuessed).size,
      symmetricDifference(audioTargetPositions ++ dualTargetPositions, audioIndexesGuessed).size
    )
  }

  def symmetricDifference[A](xs: Set[A], ys: Set[A]): Set[A] = {
    (xs diff ys) ++ (ys diff xs)
  }

}
