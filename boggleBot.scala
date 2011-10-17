import java.io.File
import java.awt.Robot
import java.awt.event._
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.matching.Regex

/***********************************************************************
 Online Boggle Games:
 http://www.searchamateur.com/Play-Free-Online-Games/Boggle-Supreme.htm
 http://www.wordplays.com/fcgi-bin/wc.pl
************************************************************************/

/*****
 Tree
*****/
class Tree(val nodes: HashMap[Char, Tree] = HashMap[Char, Tree](), var isWord: Boolean = false){
  def insertWord(word: List[Char]){
    word match{
      case Nil => isWord = true
      case head :: tail => nodes.getOrElseUpdate(head, new Tree).insertWord(tail)
    }
  }

  def getChildTree(letter: Char): Option[Tree] = {
    if(nodes.contains(letter)){
      Some(nodes(letter))
    }else{
      None
    }
  }
}//Class Tree

/***********
 GameLetter
***********/
class GameLetter(var letter: Char, var neighbors: List[GameLetter] = List[GameLetter]()){
  def addNeighbor(neighbor: GameLetter){
    neighbors = neighbor :: neighbors
  }
  override def toString: String = {
    letter.toString
  }
}//Class GameLetter

/**********
 WordTyper
**********/
class WordTyper{
  val robot = new Robot()

  def typeWords(words: List[String]){
    words.foreach(w => typeWord(w))
  }

  def typeWord(word: String){
    for(letter <- word.toUpperCase()){
      robot.keyPress(letter.toInt)
      robot.keyRelease(letter.toInt)
    }
    robot.delay(150) //small delay so we can see the words

    // press enter to submit word
    robot.keyPress(KeyEvent.VK_ENTER)
    robot.keyRelease(KeyEvent.VK_ENTER)
  }
}//class WordTyper

/*******
 Solver
*******/
class Solver(dictionary: String = "./dictionary.txt"){
  private val wordTree = new Tree
  private var board = Array.ofDim[GameLetter](4,4)
  var words: List[String] = Nil

  def init{
    print("Creating word tree from " + dictionary + "...")
    for(word <- Source.fromFile(dictionary).getLines){
      wordTree.insertWord(word.replaceAll("qu", "@").trim.toLowerCase.toList)
    }
    println("Done!")
  }//init

  def generateWords(tree: Tree, gameLetter: GameLetter, used: List[GameLetter]): List[String] = {
    tree.getChildTree(gameLetter.letter) match {
      case None => 
        Nil
      case Some(childTree) =>
        var words: List[String] = Nil
        if(childTree.isWord){
          words = (gameLetter::used).mkString("").reverse.replaceAll("@", "qu") :: words
        }
        for(next <- gameLetter.neighbors){
          if(!(gameLetter::used).contains(next)){
            words = generateWords(childTree, next, gameLetter::used) ++ words
          }
        }
        words
    }
  }//generateWords

  def solve(letters: String){
    words = Nil //reset
    //create game board
    for(i <- 0 until letters.length){
      board(i/4)(i%4) = new GameLetter(letters(i))
    }

    //add our gameLetter neighbors
    for(y <- 0 to 3; x <- 0 to 3; yOffset <- -1 to 1; xOffset <- -1 to 1){
      var off_x = x + xOffset
      var off_y = y + yOffset
      if(xOffset != 0 || yOffset != 0){ //skip ourselves :)
        if(off_x >= 0 && off_x <= 3 && off_y >= 0 && off_y <= 3){
          board(y)(x).addNeighbor(board(off_y)(off_x))
        }
      }
    }
 
    //determine all valid words
    for(x <- 0 to 3; y <- 0 to 3){
      words = words ++ generateWords(wordTree, board(x)(y), Nil)
    }
  }
}//class Solver


/*****
 Main
*****/
var running = true
var robot = new Robot()
val solver = new Solver
val wordTyper = new WordTyper
solver.init

while(running){
  var input = readLine("Type the game board letters from left to right, top to bottom: ")
  input = input.replaceAll(" ", "").replaceAll("qu", "@").trim
  if(input == "exit"){
    running = false
    println("Goodbye!")
  }else if(input.length == 16){
    print("Solving game...")
    solver.solve(input)
    print("Solved!\n\nYou have 5 seconds to focus on the input area...")
    robot.delay(5000)
    wordTyper.typeWords(solver.words)
    println("\nDone!\n")
  }else{
    println("\nInvalid game board string!")
  }
}
