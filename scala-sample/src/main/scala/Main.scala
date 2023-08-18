object Main {
  def main(args: Array[String]): Unit = {

    def formatWithMaxCharsPerLine(text: String, maxCharsPerLine: Int): List[String] = {
      val textWords = text.split(" ").toList

      // Better safe than sorry
      if (textWords.exists(_.length > maxCharsPerLine))
        throw new IllegalArgumentException("The text contains a word bigger than the line limit, " +
          "the request can't be processed.")

      // Creating a list of (String, Int) which represents the lines to be printed and the char count of the line
      textWords.foldLeft(List[(String, Int)]()) { (acc, currentWord) => acc match {
        case List() => acc :+ (currentWord, currentWord.length)
        case List(_*) :+ lastLineAndCharCount
          if lastLineAndCharCount._2 + currentWord.length + 1 > maxCharsPerLine =>
          acc :+ (currentWord , currentWord.length)
        case _ => acc.init :+ (acc.last._1 + " " + currentWord, acc.last._2 + currentWord.length + 1)
      }}.map(_._1)
    }

    val text = "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."

    formatWithMaxCharsPerLine(text, 40).foreach(println(_))

  }
}