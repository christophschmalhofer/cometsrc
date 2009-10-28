package comet.snippet

import scala.actors.Actor._  
import scala.xml.{Node,NodeSeq,Text,EntityRef}

import model.TicTacToe
import model.TicTacToe._
import comet.TicTacToeActor
import comet.TicTacToeActor._

import net.liftweb.http.{S, StatefulSnippet, SessionVar}
import net.liftweb.http.SHtml._
import net.liftweb.http.S._

import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE.JsRaw

import net.liftweb.util.{Full, Helpers, Log}


class TicTacToeGame {

  object ticTacToe extends SessionVar[TicTacToe](new TicTacToe())

  def reStart(input:NodeSeq):NodeSeq = {
    ajaxButton( Text("Neues Spiel"), () => 
      {
        ticTacToe(new TicTacToe())
        Noop & JsRaw("window.location.reload()")
      })
  }   

  def render(input:NodeSeq):NodeSeq = {
    render()
  }

  private def cellId(line:Int, column:Int):String = "cell" +line.toString + column.toString

  private def render():NodeSeq = {

    var lineIndex = -1
    
    def renderLine(l:Array[Who]): Node = {
      lineIndex += 1 
      var columnIndex = -1

      def renderCell(cell:Who):Node = { 
        columnIndex += 1
        // das sind Closure Variablen für callback
        val currentColumn = columnIndex
        val currentLine = lineIndex

        // das ist die Ajax Callback Methode (enthält die Spiellogik)
        def callback():JsCmd =  {
          Log.info("In Zelle (" + currentLine + ", " + currentColumn +") gesetzt")
          try {
            ticTacToe.is.moveToPosition((currentLine, currentColumn), who.You)
            ticTacToe.is.checkGameOver()
            computeMove()
            SetHtml("tictactoegame" , this.render()) &
            SetHtml("statusline", Text("Computer rechnet"))
          } catch {
            case ex: TicTacToe.GameOverException => { 
              displayResultBoard(ex) 
            }
          }
        }

        // eine Zelle einer Zeile
        <td width="30" height="30">
        <span id={cellId(currentLine, currentColumn)}>
        {
          if (cell == who.Empty &&  ticTacToe.is.getLastMover != who.You) {
            a(callback _, renderWho(cell))
          } else {
            renderWho(cell)
          }
        }
        </span>
        </td> 
      }
      // eine Zeile des Bretts
      <tr> 
      { 
        l.map( cell => renderCell(cell))
      }
      </tr>
    }

    // generiertes Xhtml
    <div>
    {
      renderButtonFirstMove()
    }
    <table border="1" style="font-size:300%;">
    { 
      ticTacToe.is.getBoard().map( l => renderLine(l)) 
    }
    </table> 
    </div>
  }

  private def computeMove() {
    // Zugriff auf Session noch im Snippet Thread
    val cometActor = ticTacToeActorInSession.is
    val theSession = S.session;

    actor { 
      theSession match {
        case Full(session) => {
          S.initIfUninitted(session) {
            S.functionLifespan(true) {


              Log.info("Long Computation")
              Thread.sleep(4000);
              try {
                ticTacToe.is.computeMove(who.Me);
                ticTacToe.is.checkGameOver()
                val afterMove = this.render() 
                cometActor ! afterMove
                cometActor ! SetHtml("statusline", Text("Du bist am Zug"))
              } catch {
                case ex: TicTacToe.GameOverException => { 
                  cometActor ! displayResultBoard(ex)
                }
              }

              if (S.functionMap.size > 0) {
                session.updateFunctionMap(S.functionMap, Helpers.nextFuncName, Helpers.nextNum)
                S.clearFunctionMap
              }
            }
          }
        }
        case  _ => { Log.info("Session nicht vorhanden")}
      }
    }
  }

  private def renderButtonFirstMove():Node = {
    if (ticTacToe.is.gameJustStarted()) {
      <span id="moveFirst"> 
      {
        ajaxButton( Text("Setze zuerst"), () => { computeMove(); displayBoardReadOnly()})
      }
      </span>
    } else {
      <span><br/></span>
    }
  }

  private def renderWho(whoVal:Who):Node = {
    if (whoVal == who.Empty) <span> { EntityRef("nbsp") ++ EntityRef("nbsp") ++ EntityRef("nbsp")} </span> else Text(whoVal.toString) 
  }


  //Ergebnis Spielfeld wird readonly angezeigt, Gewinnfelder werden markiert
  private def displayResultBoard(ex:GameOverException):JsCmd = {
    
    Log.info( ex.player + " hat gewonnen")
    notice( ex.player + " hat gewonnen")
    
    def partOfWinner(p:Position, triple:PositionTriple):Boolean = {
      for(i <- 0 to 2; if(triple.productElement(i).equals(p))) {
        return true
      }
      false
    }
    
    val cmds = new collection.mutable.Stack[JsCmd]
    for( l <- 0 to 2; c <- 0 to 2) {
      val who = ticTacToe.is.getCellValue((l,c))
      val renderWhoVal = ex.winnerTriple match {
        case Some(triple) => if(partOfWinner((l,c), triple)) <span style="background-color:red"> { who } </span> else renderWho(who)
        case None => renderWho(who)
      }
      cmds.push(SetHtml(cellId(l,c), renderWhoVal) )
    } 
    // siehe JsCmds: implicit def seqJsToJs(in : Seq[JsCmd])
    cmds &
    SetHtml("statusline", Text("Spiel zu Ende"))
  }          


  //Spielfeld wird readonly angezeigt
  private def displayBoardReadOnly():JsCmd = {
    val cmds = new collection.mutable.Stack[JsCmd]
    for( l <- 0 to 2; c <- 0 to 2) {
      cmds.push(SetHtml( cellId(l,c), renderWho(ticTacToe.is.getCellValue((l,c)))))
    } 
    // siehe JsCmds: implicit def seqJsToJs(in : Seq[JsCmd])
    cmds & SetHtml("statusline", Text("Computer rechnet"))
  }          
}

