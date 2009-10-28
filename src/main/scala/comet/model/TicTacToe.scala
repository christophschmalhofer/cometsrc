package comet.model

//TicTacToe in Konsole

//Build: scalac -deprecation TicTacToe.scala
//Run: scala TicTacToe star

import java.util.Calendar;
import _root_.net.liftweb.util.Log
import TicTacToe._


object TicTacToe {

  // wer hat in eine Zelle gesetzt
  // wer hat gewonnen
  object who extends Enumeration {
    val Empty = Value("_")
    val Me = Value("o")
    val You = Value("x");
    val Nobody = Value("Niemand");
  }


  type Who = who.Value
  type Position = (Int,Int)
  type PositionTriple = (Position,Position,Position)

    // falls Spiel entschieden -> Ausstieg aus Eingabeschleife
    // falls Gewinnerposition ermittelt -> Ausstieg aus Suche
    case class GameOverException(
      val player:who.Value, 
      //das ist der ermittelte Zug (direkter oder indirekter Siegeszug)
      val nextMove:Option[Position],
      val winnerTriple:Option[PositionTriple]
    ) extends Exception

}

class TicTacToe {

  //Spielfeld: 3x3 Array
  private val master = new Array[Array[Who]](3,3)
  for(l <- master; i <- 0 until l.length) {
    l(i) = who.Empty
  }

  def getBoard():Array[Array[Who]] = clone(master)

  private var lastMover = who.Empty

  def getLastMover():Who = lastMover
 
  //Liste von Gewinnerpositionen
  private val winnerTriples = List[PositionTriple](
    //Zeilen
    ((0,0),(0,1),(0,2)), 
    ((1,0),(1,1),(1,2)), 
    ((2,0),(2,1),(2,2)),
    //Spalten
    ((0,0),(1,0),(2,0)), 
    ((0,1),(1,1),(2,1)), 
    ((0,2),(1,2),(2,2)),
    //Diagonalen
    ((0,0), (1,1), (2,2)), 
    ((0,2), (1,1), (2,0)))

  // Dient zum Parsen der Konsoleneingabe für Zug in eine Zelle
  // Exctractor: "l,c" => Paar 
  private object Cell {
    def unapply(str: String): Option[Position] = { 
      val parts = str split ","
      if (parts.length == 2) Some((parts(0).toInt, parts(1).toInt)) else None
    } 
  }

  def gameJustStarted():Boolean =  emptyCells(master).length == master.length * master(0).length
  
  // Siegstellung oder Remis => GameOverException
  def checkGameOver() {
    checkIsWinner(None,master, (0,0))
    if (emptyCells(master).isEmpty) {
      throw new GameOverException(who.Nobody, None, None)
    }
  }

  // player hat gewonnen => GameOverException
  // player = None => hat jemand gewonnen?
  private def checkIsWinner(player:Option[Who], field:Array[Array[Who]], nextMove:Position) {
    
    def isWinner(triple:PositionTriple):Boolean = {
      getWho(field, triple._1) != who.Empty  && getWho(field, triple._2) == getWho(field, triple._1) && 
      getWho(field, triple._3) == getWho(field,triple._1) && 
      (player.isEmpty || getWho( field,triple._1) == player.get())
    }
    
    winnerTriples.find(isWinner(_)) match {
      case Some(triple) => throw new GameOverException(getWho(field, triple._1), Some(nextMove), Some(triple))
      case None => {}
    }
  }

  // Programm zieht
  def computeMove(player:Who):Position = {
    // habe ich einen Siegeszug ?
    try {
      searchWinner(player, master)
    } catch {
      case ex: GameOverException => val move = ex.nextMove.getOrElse(emptyCells(master).first); setWho(master, move, player);return move 
    }

    // hast du einen Siegeszug ?
    try {
      searchWinner(getOtherPlayer(player), master)
    } catch {
      //vermassle seinen Siegeszug 
      case ex: GameOverException => val move = ex.nextMove.getOrElse(emptyCells(master).first); setWho(master, move, player);return move
    }
    val emptyCellsV =  emptyCells(master)
    val rand = new Random(Calendar.getInstance().getTimeInMillis())
    val move = emptyCellsV(rand.nextInt(emptyCellsV.length))
    setWho(master, move, player)    
    move
  }
  
  // liefert Gegner von player
  def getOtherPlayer(player:Who):Who = {
    if (player.equals(who.Me)) who.You else who.Me
  }

  
  // das ist ein Min-Max inspirierter Algorithmus
  // die Minumum Ermittlung ist aber naiv, man hat also eine Chance (wenn man selbst anfängt)
  private def searchWinner(player:Who, field:Array[Array[Who]]) {

    // player setzt in dem geklonten Spielstand in die Zelle cell 
    def cloneAndMoveAndCheckIsWinner(player:Who, cell:Position) { 
      val cloneVal = clone(field)
      setWho(cloneVal, cell, player)
      checkIsWinner(Some(player), cloneVal, cell)
    }

    // zuerst einfachen Siegeszug suchen
    emptyCells(field).foreach(cloneAndMoveAndCheckIsWinner(player, _))

    // Abbruchbedingung: falls Gegner einfachen Siegeszug hat -> hilft kein indirekter
    try {
      emptyCells(field).foreach(cloneAndMoveAndCheckIsWinner(getOtherPlayer(player), _))
    } catch {
      case ex: GameOverException => {return}
    }

    // Abbruchbedingung: kann indirekten Siegeszug nur geben, wenn noch mindestens drei Felder leer
    if (emptyCells(field).length <= 3) {
      return;
    }

    // indirekten Siegeszug suchen 
    for( myCell <-emptyCells(field)) {
      val cloneVal = clone(field)
      setWho(cloneVal, myCell, player)
      // gibt es für jeden gegnerischen Zug einen direkten Siegerzug
      // oder indirekten Siegerzug (mit Einschränkung: kein kürzerer Siegeszug des Gegners) 
      // handelt es sich um einen indirekten
      // Siegerzug
      var alwaysWinner = true
      for( emptyCell <-emptyCells(cloneVal)) {
        val cloneClone = clone(cloneVal)
        setWho(cloneClone, emptyCell, getOtherPlayer(player))
        try {
          searchWinner(player, cloneClone)
          alwaysWinner = false;
        } catch {
          case ex: GameOverException => {}
          //alwaysWinner bleibt true;
        }
      }
      if (alwaysWinner) {
        //indirekten Siegeszug gefunden
        throw new GameOverException(player, Some(myCell), None)
      }
    }
  }
  
  // liefert eine geklontes Spielfeld (Besetzung identisch zu Parameter field)
  private def clone(field:Array[Array[Who]]):Array[Array[Who]] = { 
    return field.map( l => l.map( v => v))
  }
  
  // welcher player hat in die Zelle position des Feldes gesetzt
  private def getWho(field:Array[Array[Who]], position:Position): Who = {
    field(position._1)(position._2)
  }

  // welcher player hat in die Zelle position des Master Feldes gesetzt
  def getCellValue(position:Position): Who = {
    master(position._1)(position._2)
  }

  // player setzt in Zelle t
  private def setWho(field:Array[Array[Who]], position:Position, player:Who) {
    field(position._1)(position._2) = player
    if (field eq master) {
      lastMover = player
      Log.info ("Last Mover: " + player)
    }
  }

  def moveToPosition(position:Position, player:Who) {
    checkGameOver()
    setWho(master, position, player)
    checkGameOver()
  }
  
  // liefert freie Zellen
  private def emptyCells(field:Array[Array[Who]]):Seq[Position] = {
    for (
      l <- 0 until master.length; 
      c <- 0 until master(1).length
      if who.Empty.equals(field(l)(c))
    ) yield(l,c)
  }
}
