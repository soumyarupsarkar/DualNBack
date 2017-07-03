package xyz.shoom.dualnback

object DNBConfig {

  val startingNBackLevel: Int = 2

  def numTrials(nBackLevel: Int): Int = 20 + nBackLevel

  val stimulusLength: Long = 500L
  val interstimulusInterval: Long = 2500L
  val oneModalityAuditoryTargets = 4
  val oneModalityVisualTargets = 4
  val bothModalityAuditoryAndVisualTargets = 2

  val numBlocks = 20 // TODO: do we want to use this?

  // TODO: for now, for a "Jaeggi mode" approximation, just make these false
  val (feedbackOnError: Boolean, feedbackOnKeyPress: Boolean) = (true, true)
}
