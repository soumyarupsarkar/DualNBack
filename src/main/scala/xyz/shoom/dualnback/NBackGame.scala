package xyz.shoom.dualnback

import cats.data._, cats.implicits._

import scala.util.Random

object NBackGame {
  final case class Stimulus(visual: Int, audio: Char)

  sealed trait MatchAttempt
  final case class VisualMatchAttempt(idx: Int) extends MatchAttempt
  final case class AudioMatchAttempt(idx: Int) extends MatchAttempt

  val possibleVisuals: Vector[Int] = (0 to 8).toVector.filterNot(_ == 4) // position going from left to right, top down
  val possibleAudio: Vector[Char] = "cghkpqtw".toVector

  def randElemFromArray[A](xs: Vector[A]): State[Random, A] = State(r => (r, xs(r.nextInt(xs.length))))
  def shuffleSeq[A](xs: Seq[A]): State[Random, Seq[A]] = State(r => (r, r.shuffle(xs)))

  def generateSignal(visuals: Vector[Int], audio: Vector[Char]): State[Random, Stimulus] =
    for {
      v <- randElemFromArray(visuals)
      c <- randElemFromArray(audio)
    } yield Stimulus(v, c)

  def generateNonTargetSignals(numTrials: Int, nBackLevel: Int): State[Random, List[Stimulus]] = {
    def go(acc: State[Random, List[Stimulus]]) = {
      for {
        lst <- acc
        futureStimulus = lst.lift(nBackLevel - 1)
        stimulus <- generateSignal(possibleVisuals.filterNot(x => futureStimulus.exists(_.visual == x)), possibleAudio.filterNot(x => futureStimulus.exists(_.audio == x)))
      } yield stimulus :: lst
    }
    def go2(acc: State[Random, List[Stimulus]]): State[Random, List[Stimulus]] = acc.flatMap(lst => if (lst.size == numTrials) State.pure(lst) else go2(go(acc)))
    val seed = generateSignal(possibleVisuals, possibleAudio).map(x => List(x))
    go2(seed)
  }

  // TODO: consider parameterizing the number of visual, audio, and dual targets
  def generateTargetPositions(numTrials: Int, nBackLevel: Int): State[Random, (Set[Int], Set[Int], Set[Int])] = {
    for {
      shuffledPositions <- shuffleSeq(nBackLevel until numTrials)
      visualTargetPositions = shuffledPositions.slice(0, 4).toSet
      audioTargetPositions = shuffledPositions.slice(4, 8).toSet
      dualTargetPositions = shuffledPositions.slice(8, 10).toSet
    } yield (visualTargetPositions, audioTargetPositions, dualTargetPositions)
  }

  def generateSignalsWithTargets(
    nonTargetSignals: List[Stimulus],
    visualTargetPositions: Set[Int],
    audioTargetPositions: Set[Int],
    dualTargetPositions: Set[Int],
    nBackLevel: Int
  ): Seq[Stimulus] = {
    nonTargetSignals.indices.foldLeft(List.empty[Stimulus]){(acc, idx) =>
      if (visualTargetPositions.contains(idx)) Stimulus(acc(nBackLevel-1).visual, nonTargetSignals(idx).audio) :: acc
      else if (audioTargetPositions.contains(idx)) Stimulus(nonTargetSignals(idx).visual, acc(nBackLevel-1).audio) :: acc
      else if (dualTargetPositions.contains(idx)) Stimulus(acc(nBackLevel-1).visual, acc(nBackLevel-1).audio) :: acc
      else nonTargetSignals(idx) :: acc
    }.reverse
  }

  /**
    * Generate signals with targets placed at indexes in the (visual, audio, dual) tuple.
    *
    * NOTE: there is a problem with this approach. We start with a game with no targets, then as we build up a new list,
    * we insert targets. Here's what could go wrong: we could insert an audio match at position 7, changing it from (3,h)
    * to (3,g), then if we insert a visual match at position 9, changing it from (7,g) to (3,g), we have just inserted
    * a dual match. These are accidental, uncounted dual matches.
    *
    * For N=2, this happens for approximately 25% of all calls to this function:
    * scala.collection.immutable.Stream.continually(generateSignalsAndTargets(22, 2).run(new Random()).value._2.some
    * .map(xx => xx.some.exists(x => x._2._1.exists(y => x._1(y) == x._1(y-2))) || xx.some.exists(x => x._2._2
    * .exists(y => x._1(y) == x._1(y-2))))).take(100000).toSeq.filter(_.contains(true)).size
    *
    * It happens less often at higher N (around 19% at N=8).
    *
    * TODO: properly fix this if possible
    */
  def generateSignalsAndTargets(numTrials: Int, nBackLevel: Int): State[Random, (Seq[Stimulus], (Set[Int], Set[Int], Set[Int]))] = {
    generateNonTargetSignals(numTrials, nBackLevel)
      .flatMap(nonTargetSignals =>
        generateTargetPositions(numTrials, nBackLevel)
          .map(targetPositions =>
            generateSignalsWithTargets(nonTargetSignals, targetPositions._1, targetPositions._2, targetPositions._3, nBackLevel) -> targetPositions
          )
      )
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
