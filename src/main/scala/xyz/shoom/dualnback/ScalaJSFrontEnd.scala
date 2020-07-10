package xyz.shoom.dualnback

import cats.data._
import cats.implicits._
import org.scalajs.dom
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{AudioContext, KeyboardEvent, MouseEvent, XMLHttpRequest}
import org.scalajs.dom.{CanvasRenderingContext2D, Event, html}
import scalatags.JsDom.all._
import xyz.shoom.dualnback.NBackGame.{AudioMatchAttempt, MatchAttempt, Stimulus, VisualMatchAttempt}

import scala.annotation.tailrec
import scala.scalajs.js.URIUtils
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.timers._
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.util.{Random, Try}

@JSExportTopLevel("ScalaJSFrontEnd")
object ScalaJSFrontEnd {

  val dnbAudioPackLocalStorageKey = "dnbAudioPack"
  val audioPackNames = List("Gedgaudas", "Sarkar")
  val defaultAudioPack = "Gedgaudas"

  @JSExport
  def main(target: html.Div): Unit = {
    println("ScalaJSFrontEnd Initialized")

    val queryParameters =
      dom.window.location.search
        .drop(1)
        .split("&")
        .map(_.split("="))
        .filterNot(_.length != 2)
        .map(xs => xs(0) -> xs(1))
        .map(x => URIUtils.decodeURIComponent(x._1) -> URIUtils.decodeURIComponent(x._2))
        .toMap

    val nBackLevelOption = Try{val l = queryParameters("dualNBackLevel").toInt; if (l < 1) throw new RuntimeException(); l}.toOption
    val mistakesMadeOption = Try(queryParameters("visualMistakes").toInt -> queryParameters("audioMistakes").toInt).toOption

    (nBackLevelOption, mistakesMadeOption) match {
      case (Some(nBackLevel), Some((visualMistakes, audioMistakes))) =>
        dom.document.body.style = "font-size: 1.5rem; margin-top: 1.25rem; margin-left: 2rem; font-family: Helvetica Neue,Helvetica,sans-serif; line-height: 1.5;"
        dom.document.body.style.backgroundColor = "#222233"
        dom.document.body.style.color = "#AACCFF"
        val recommendIncreasingN = visualMistakes < 3 && audioMistakes < 3
        val recommendDecreasingN = nBackLevel >= 2 && (visualMistakes + audioMistakes) > 5
        target.appendChild(
          div(
            h1("Dual N-Back"),
            p(
              "On Dual ", b(nBackLevel), "-Back, you made ",  b(visualMistakes), " visual match mistakes and ",
              b(audioMistakes), " audio match mistakes."
            ),
            p(
              if (recommendIncreasingN)
                s"Because you made fewer than 3 mistakes per modality, we recommend increasing N."
              else if (recommendDecreasingN)
                s"Because you made more than 5 mistakes, we recommend decreasing N."
              else
                s"Try again."
            )
          ).render
        )
        appendNBackLevelSubmitBox(target)
      case (Some(nBackLevel), None) =>
        dom.document.body.style = "margin-top: 0rem; margin-left: 0rem; margin-bottom: 0rem; overflow: hidden; position: fixed;"
        target.appendChild(canvas(id:="canvas").render)
        val audioContext = new AudioContext
        val gameCanvas = dom.document.getElementById("canvas") match { case c: html.Canvas => c }
        val renderer = gameCanvas.getContext("2d") match { case ctx: dom.CanvasRenderingContext2D => ctx }

        println(s"Using Dual N-Back Level: ${nBackLevel}")

        val (stimuli, (visualMatchIdxs, audioMatchIdxs, dualMatchIdxs)) = generateGame(nBackLevel)

        println(s"Stimuli to be presented: ${stimuli.zipWithIndex}")
        println(s"Just-visual match indexes: ${visualMatchIdxs} | Just-audio match indexes: ${audioMatchIdxs} | Dual match indexes: ${dualMatchIdxs}")

        gameCanvas.height = dom.document.documentElement.clientHeight
        gameCanvas.width = dom.document.documentElement.clientWidth

        clearGrid(gameCanvas, renderer)
        val matchAttempts = collection.mutable.Queue.empty[MatchAttempt]
        renderStimuli(stimuli.toList, 0, nBackLevel, gameCanvas, audioContext, renderer, matchAttempts, visualMatchIdxs, audioMatchIdxs, dualMatchIdxs)
      case _ =>
        dom.document.body.style = "font-size: 1.5rem; margin-top: 1.25rem; margin-left: 2rem; font-family: Helvetica Neue,Helvetica,sans-serif; line-height: 1.5;"
        dom.document.body.style.backgroundColor = "#222233"
        dom.document.body.style.color = "#AACCFF"
        target.appendChild(
          div(
            h1("Dual N-Back"),
            p(
              "This is an implementation of the Dual N-Back task described in \"Improving fluid intelligence with " +
                "training on working memory\" (Jaeggi et al., 2008)."
            ),
            p("For 20 + N trials, you will see a square on a 3x3 grid, and simultaneously you will hear a letter."),
            p(
              "At each trial:", br(),
              "If the square is in the same position as it was N trials ago, press ", b("A"), " or click on the left side of the grid.", br(),
              "If the letter you hear is the same letter you heard N trials ago, press ", b("L"), " or click on the right side of the grid."
            ),
            p("Note that both may be identical to what they were N trials ago."),
            p("Enter the level (the N in Dual N-Back) you would like to start at. We recommend starting at level 2.")
          ).render
        )
        appendNBackLevelSubmitBox(target)
        appendAudioPackButtons(target)
        val linkColor = "#CA6754" // could also be #BA9B63 or #C1542E
        target.appendChild(
          div(
            p(
              "See ", a(href:="http://www.gwern.net/DNB-FAQ",rel:="noopener",color:=linkColor)("Gwern Branwen's FAQ"),
              " for an overview of research related to the Dual N-Back task."
            ),
            p(
              "View the source code (or file an issue) at ",
              a(href:="https://github.com/soumyarupsarkar/DualNBack",rel:="noopener",color:=linkColor)("soumyarupsarkar/DualNBack"),
              " on GitHub."
            ),
            p(
              "The Gedgaudas sound pack of letters of the alphabet is credited to Amy Gedgaudas and was distributed by Tim Kahn " +
              "on ", a(href:="https://freesound.org/people/tim.kahn/packs/4371/",rel:="noopener",color:=linkColor)("freesound"), " under ",
                a(href:="https://creativecommons.org/licenses/by/3.0/",rel:="noopener",color:=linkColor)("CC BY 3.0"), "."
            )
          ).render
        )
    }
  }

  // retrying is necessary due to the behavior of generateSignalsAndTargets possibly returning unidentified/accidental dual matches
  // TODO: this was insufficient but should no longer be necessary anyway. remove after adding tests that confirm there are no uncounted dual matches.
  @tailrec
  def generateGame(nBackLevel: Int): (Seq[Stimulus], (Set[Int], Set[Int], Set[Int])) = {
    val game = NBackGame.generateSignalsAndTargets(DNBConfig.numTrials(nBackLevel), nBackLevel).run(new Random()).value._2
    if (
      game.some.exists(x => x._2._1.exists(y => x._1(y) == x._1(y-nBackLevel))) ||
      game.some.exists(x => x._2._2.exists(y => x._1(y) == x._1(y-nBackLevel)))
    ) generateGame(nBackLevel)
    else game
  }

  def appendNBackLevelSubmitBox(target: Div): Unit = {
    val nBackLevelBox =
      input(
        style := "display: inline-block; padding: 10px 15px; border-radius: 0; border: 1px solid transparent;",
        `type` := "text",
        placeholder := "N-Back Level"
      ).render
    val startButton =
      input(
        // #C9D7DE is also a nice color, so is #CF5D52, and maybe #DADEE0 or #ADA9A0
        style := "display: inline-block; padding: 10px 15px; border-radius: 0; border: 1px solid transparent; background-color: #DADEE0;",
        `type` := "button",
        value := "Start"
      ).render
    val errorRegion = span(color:="#CC0000").render
    target.appendChild(
      div(
        nBackLevelBox,
        startButton,
        errorRegion
      ).render
    )

    def redirectToGame(): Unit = {
      if (Try(nBackLevelBox.value.toInt).filter(_ >= 1).isSuccess)
        dom.window.location.search = s"?dualNBackLevel=${URIUtils.encodeURIComponent(nBackLevelBox.value)}"
      else
        errorRegion.innerHTML = " Error: N-Back level needs to be a number greater than zero."
    }

    nBackLevelBox.onkeypress = (e: KeyboardEvent) => e.keyCode match {
      case 13 => redirectToGame()
      case _ => ()
    }
    startButton.onclick = (e: MouseEvent) => redirectToGame()
  }

  private def appendAudioPackButtons(target: Div): Unit = {
    val selectedAudioPack = getSelectedDnbAudioPackFromLocalStorage()

    val audioPackRadioButtons =
      audioPackNames.map(packName => packName ->
        span(
          onclick := ((e: MouseEvent) => dom.window.localStorage.setItem(dnbAudioPackLocalStorageKey, packName)),
          input(id := "audioPack" + packName, `type` := "radio", name := "audioPack", value := packName,
            if (packName == selectedAudioPack) checked else placeholder := ""),
          label(`for` := "audioPack" + packName, packName)
        )
      )
    target.appendChild(
      div(
        p(
          "Sound pack:", span(audioPackRadioButtons.map(_._2): _*)
        )
      ).render
    )
  }

  def renderStimuli(
    stimuli: List[Stimulus],
    idx: Int,
    nBackLevel: Int,
    gameCanvas: html.Canvas,
    audioContext: AudioContext,
    renderer: dom.CanvasRenderingContext2D,
    matchAttempts: collection.mutable.Queue[MatchAttempt],
    visualMatchIdxs: Set[Int],
    audioMatchIdxs: Set[Int],
    dualMatchIdxs: Set[Int]
  ): Unit = {
    if (stimuli.nonEmpty) {
      renderVisual(gameCanvas, renderer, stimuli.head.visual)
      val selectedAudioPack = getSelectedDnbAudioPackFromLocalStorage()
      playSound("audio/" + selectedAudioPack + "/" + stimuli.head.audio + ".wav", audioContext)
      gameCanvas.style.outline = "none"
      gameCanvas.tabIndex = 1000
      gameCanvas.focus()
      gameCanvas.onkeypress = (e: dom.KeyboardEvent) => {
        e.key match {
          case "a" => matchAttempts.enqueue(VisualMatchAttempt(idx))
          case "l" => matchAttempts.enqueue(AudioMatchAttempt(idx))
          case _ => ()
        }
      }
      gameCanvas.onclick = (e: dom.MouseEvent) => {
        e.clientX match {
          case x if x < gameCanvas.width / 2 => matchAttempts.enqueue(VisualMatchAttempt(idx))
          case x if x > gameCanvas.width / 2 => matchAttempts.enqueue(AudioMatchAttempt(idx))
        }
      }
      setTimeout(DNBConfig.stimulusLength)(clearGrid(gameCanvas, renderer))
      setTimeout(DNBConfig.stimulusLength + DNBConfig.interstimulusInterval)(
        renderStimuli(stimuli.drop(1), idx+1, nBackLevel, gameCanvas, audioContext, renderer, matchAttempts, visualMatchIdxs, audioMatchIdxs, dualMatchIdxs)
      )
    } else {
      val matchAttemptSet = matchAttempts.toSet
      val (visualMistakesCount, audioMistakesCount) =
        NBackGame.countMistakes(visualMatchIdxs, audioMatchIdxs, dualMatchIdxs, matchAttemptSet)
      println(s"Your match attempts: ${matchAttemptSet}")
      println(s"Visual mistakes: ${visualMistakesCount}\nAudio mistakes: ${audioMistakesCount}")
      println("Jaeggi 2008: If the participant made fewer than three mistakes per modality, the level of n increased " +
        "by 1. It was decreased by 1 if more than five mistakes were made, and in all other cases, n remained " +
        "unchanged.")
      dom.window.location.search = s"?dualNBackLevel=${nBackLevel}" +
        s"&visualMistakes=${visualMistakesCount}" +
        s"&audioMistakes=${audioMistakesCount}"
    }
  }

  def getSelectedDnbAudioPackFromLocalStorage(): String =
    Option(dom.window.localStorage.getItem(dnbAudioPackLocalStorageKey))
      .filter(audioPackNames.contains)
      .getOrElse(defaultAudioPack)

  def playSound(url: String, audioContext: AudioContext): Unit = {
    val source = audioContext.createBufferSource()
    val xhr = new XMLHttpRequest
    xhr.open("GET", url)
    xhr.responseType = "arraybuffer"
    xhr.addEventListener("load", (e: Event) => {
      xhr.response match {
        case xhrResponse: ArrayBuffer =>
          audioContext.decodeAudioData(
            xhrResponse,
            buffer => {
              source.buffer = buffer
              source.connect(audioContext.destination)
              source.loop = false
            }
          )
          source.start(0)
      }
    })
    xhr.send()
  }

  def renderVisual(gameCanvas: html.Canvas, renderer: CanvasRenderingContext2D, visualPosition: Int): Unit = {
    renderer.fillStyle = "#7f7fff"
    renderer.fillRect((visualPosition % 3) * gameCanvas.width / 3.0, visualPosition / 3 * gameCanvas.height / 3.0, gameCanvas.width / 3.0, gameCanvas.height / 3.0)
  }

  def clearGrid(gameCanvas: html.Canvas, renderer: dom.CanvasRenderingContext2D): Unit = {
    renderer.clearRect(0, 0, gameCanvas.width.toDouble, gameCanvas.height.toDouble)
    renderer.fillStyle = "#000000"
    renderer.fillRect(0, 0, gameCanvas.width.toDouble, gameCanvas.height.toDouble)

    renderer.strokeStyle = "#ffffff"
    renderer.beginPath()
    renderer.moveTo(gameCanvas.width / 3.0, 0.0)
    renderer.lineTo(gameCanvas.width / 3.0, gameCanvas.height.toDouble)
    renderer.stroke()
    renderer.beginPath()
    renderer.moveTo(2 * gameCanvas.width / 3.0, 0.0)
    renderer.lineTo(2 * gameCanvas.width / 3.0, gameCanvas.height.toDouble)
    renderer.stroke()
    renderer.beginPath()
    renderer.moveTo(0.0, gameCanvas.height / 3.0)
    renderer.lineTo(gameCanvas.width.toDouble, gameCanvas.height / 3.0)
    renderer.stroke()
    renderer.moveTo(0.0, 2 * gameCanvas.height / 3.0)
    renderer.lineTo(gameCanvas.width.toDouble, 2 * gameCanvas.height / 3.0)
    renderer.stroke()
  }
}
