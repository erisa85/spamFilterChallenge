import scala.collection.mutable.HashMap

///////////////// HARDER VERSION OF THE SPAM FILTER CHALLENGE //////////////////

/** Marks something you should complete to finish this challenge. */
object TODO {
  def apply (msg: String): Nothing =
    throw new AssertionError("Not finished: " + msg)
}

/** Utility for extracting words. */
object Words {
  /** Whether a character is whitespace, for the purposes of this challenge. */
  def isWhite (ch: Char) = Character.isWhitespace(ch)
  /** Converts a character sequence to a stream of words. */
  def apply (chars: Seq[Char]): Stream[String] = {
    if (chars.isEmpty)
      Stream.empty
    else if (isWhite(chars.head))
      apply(chars.dropWhile(isWhite(_)))
    else{
      val (word,rest) = chars.span(! isWhite(_))
      Stream.cons(word.foldLeft("")(_ + _), apply(rest))
    }

  }
}

/**
 * A group of training examples. Messages are delimited by newlines, words by
 * whitespace. Empty messages (those with no words) are not counted.
 *
 * Extra credit!: In this solution, an `Examples` holds on to the `messages`
 * stream it is given (a case class's parameters are effectively `public`, so
 * Scala is not free to discard them once it finishes constructing the object
 * to which they are passed). However, the original stream is not needed once
 * an `Examples` has extracted the required information from it. How can you
 * change this class so that unneeded values are recognized as garbage?
 */
case class Examples (messages: Stream[Char]) {
  val lines = getLines(messages, List[List[String]]())
  private def getLines(messages: Stream[Char], lines: List[List[String]]): List[List[String]] = {
  messages.span(char => char !='\n') match  {
      case (Stream(), Stream()) => lines
      case(currentLine, Stream()) => lines :+ currentLine.mkString.split(" ").toList
      case(currentLine, restOfLines) => getLines(restOfLines.drop(1), lines :+ currentLine.mkString.split(" ").toList)
    }
  }

  private def getWordsFrequencyHash(wordsToFrequency: HashMap[String, Int], wordsList: List[String]): HashMap[String, Int] =
  {
    wordsList.foreach(word =>
        wordsToFrequency.put(word, wordsToFrequency.getOrElse(word, 0) + 1))
    wordsToFrequency
  }

  val wordsFrequencyHashMap = lines.foldLeft(HashMap[String, Int]())(getWordsFrequencyHash(_,_))

  /** How many messages in `messages`. */
  val messageCount: Int = lines.size
  println("number of msg " + messageCount)

  /** How many words (including duplicates) in `messages`. */
  val wordCount: Int = wordsFrequencyHashMap.values.toList.foldLeft(0)(_ + _)
  println("number of words " + wordCount)
  /** All the words in `messages`. */
  lazy val dictionary: Set[String] = wordsFrequencyHashMap.keySet.toSet
  /** The occurrence count for a word in `messages` (zero if not present). */
  def occurrences (word: String): Int = wordsFrequencyHashMap.getOrElse(word, 0)
}

/**
 * A filter trained by examples of spam and ham (where &ldquo;ham&rdquo; means
 * &ldquo;not spam&rdquo;).
 */
case class Filter (spam: Examples, ham: Examples) {
  /** Laplacian smoother. */
  val laplaceSmoother = 1.0
  /**
   * The probability that the given message is spam, as a number between zero
   * and one.
   */
  val smoothedMessagesCount = spam.messageCount + ham.messageCount + 2 * laplaceSmoother
  val spamProbability = (spam.messageCount + laplaceSmoother)/smoothedMessagesCount
  val hamProbability =(ham.messageCount + laplaceSmoother)/smoothedMessagesCount
  val dictionarySize = spam.dictionary.size + ham.dictionary.size
  def probabilityGivenClassifier(word: String, classifier: Examples): Double = {
    (classifier.occurrences(word) + laplaceSmoother)/(classifier.wordCount + laplaceSmoother * dictionarySize)
  }

  def isSpam (message: Seq[Char]): Double = {
    val words = Words(message)
    val spamProbabilityGivenWords = words.foldLeft(spamProbability)(_ * probabilityGivenClassifier(_, spam))
    val hamProbabilityGivenWords = words.foldLeft(hamProbability)(_ * probabilityGivenClassifier(_, ham))
    spamProbabilityGivenWords/(spamProbabilityGivenWords + hamProbabilityGivenWords)
  }
  override def toString: String = "spamProb = %3.2f%%   hamProb = %3.2f%%   dictSize = %d".format(
    spamProbability * 100.0,hamProbability * 100.0,dictionarySize)
}

/**
 * Program that prompts for spam and ham training files, then classifies
 * messages you enter.
 */
object Spam extends App {
  def promptAndRead (prompt: String): String = {
    print(prompt)
    readLine
  }
  import scala.io.BufferedSource
  import java.io.FileInputStream
  def withFile [X] (prompt: String, whatToDo: BufferedSource => X): X = {
    val bs =
      new BufferedSource(new FileInputStream(promptAndRead(prompt + ": ")))
    try
      whatToDo(bs)
    finally
      bs.close
  }
  val filter =
    Filter(
      Examples(withFile("Spam example file",_.toStream)),
      Examples(withFile("Ham example file",_.toStream)))
  println("\nResulting filter: " + filter)
  println
  def run (msg: String): Unit =
    if (msg.isEmpty) {
      val firstLine = promptAndRead("Message to classify (newline to end):\n")
      if (! firstLine.isEmpty) run(firstLine)
    } else {
      val nextLine = readLine
      if (nextLine.isEmpty) {
        printf("Spam likelihood: %3.2f%%\n\n",100.0 * filter.isSpam(msg))
        run("")
      } else
        run(msg + " " + nextLine)
    }
  run("")
}
