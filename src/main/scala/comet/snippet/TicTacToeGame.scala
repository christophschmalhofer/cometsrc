package comet.snippet

import scala.actors.Actor._  
import scala.xml.{Node,NodeSeq,Text,EntityRef}

import model.TicTacToe
import model.TicTacToe._
import comet.TicTacToeActor
import TicTacToeActor._

import net.liftweb.http.{S, StatefulSnippet, SessionVar}
import net.liftweb.http.SHtml._
import S._

import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE.JsRaw

import net.liftweb.util.{Full, Helpers, Log, HttpHelpers}
import net.liftweb.util.Helpers._

import net.liftweb.http.js.JE
import JE._
import net.liftweb.http.js.jquery._ 
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.http.js.jquery.JqJE._

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
            SetHtml("tictactoegame" , this.render())
          } catch {
            case ex: TicTacToe.GameOverException => { 
              displayResultBoard(ex) 
            }
          }
        }

        // eine Zelle einer Zeile
        val td = <td width="30" height="30" id={cellId(currentLine, currentColumn)}>{ renderWho(cell)}</td>
        if ( cell == who.Empty &&  ticTacToe.is.getLastMover != who.You && ! ticTacToe.is.gameOver) {
          td % ("onclick" -> {ajaxInvoke(callback _)._2}) % ("style" -> "background-color:LightSkyBlue;cursor: crosshair") % ("title" -> "click")
        } else {
          td
        }
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
    <p id="statusline" > { renderStatusLine() } </p>
    </div>
  }

  private def computeMove() {
    // session access in snippet thread (otherwise -> session not found)
    val theSession = S.session;

    actor { 
      theSession match {
        case Full(session) => {
          S.initIfUninitted(session) {
            S.functionLifespan(true) {
              Log.info("Long Computation")
              Thread.sleep(4000);
              val cometActor = ticTacToeActorInSession.is
              try {
                ticTacToe.is.computeMove(who.Me);
                ticTacToe.is.checkGameOver()
                val afterMove = this.render() 
                cometActor ! afterMove
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
        case  _ => { Log.info("session not found")}
      }
    }
  }

  private def renderButtonFirstMove():Node = {
    if (ticTacToe.is.gameJustStarted()) {
      <span id="moveFirst"> 
      {
        ajaxButton( Text("Computer setzt zuerst"), () => { computeMove(); displayBoardReadOnly()})
      }
      </span>
    } else {
      <span><br/></span>
    }
  }

  private def renderWho(whoVal:Who):Node = {
    if (whoVal == who.Empty) { EntityRef("nbsp")} else Text(whoVal.toString) 
  }


  //Ergebnis Spielfeld wird readonly angezeigt, Gewinnfelder werden markiert
  private def displayResultBoard(ex:GameOverException):JsCmd = {
    
    Log.info( ex.player + " hat gewonnen")
    
    def partOfWinner(p:Position, triple:PositionTriple):Boolean = {
      for(i <- 0 to 2; if(triple.productElement(i).equals(p))) {
        return true
      }
      false
    }
    
    val cmds = new collection.mutable.Stack[JsCmd]
    for( l <- 0 to 2; c <- 0 to 2) {
      val who = ticTacToe.is.getCellValue((l,c))
      val noOnclickCmd = JqId(JE.Str(cellId(l,c))) >> JqHtml(renderWho(ticTacToe.is.getCellValue((l,c)))) >> JqAttr("onclick", "")
      val cmd = ex.winnerTriple match {
        case Some(triple) => {
          if(partOfWinner((l,c), triple)) {
            noOnclickCmd  >> JqAttr("style", "background-color: red") 
          } else {
            noOnclickCmd >> JqAttr("style", "") 
          }
        }
        case None => noOnclickCmd >> JqAttr("style", "") 
      }
      cmds.push(cmd)
    } 
    cmds & SetHtml("statusline", renderStatusLine())
  }          


  //Spielfeld wird readonly angezeigt
  private def displayBoardReadOnly():JsCmd = {
    val cmds = new collection.mutable.Stack[JsCmd]
    for( l <- 0 to 2; c <- 0 to 2) {
      val updateCellCmd = JqId(JE.Str(cellId(l,c))) >> JqHtml(renderWho(ticTacToe.is.getCellValue((l,c)))) >> JqAttr("onclick", "") >> JqAttr("style", "")
      cmds.push(updateCellCmd)
    } 
    //das Modell kann beim ersten Zug nicht wissen, dass der Computer dran ist
    cmds & SetHtml("statusline", renderStatusLineForStatus(Status.MY_MOVE))
  }

  private def renderStatusLine():Node = {
    renderStatusLineForStatus(ticTacToe.is.getStatus)
  }

  private def renderStatusLineForStatus(status:Status.Value):Node = {
    status match {
      case Status.FRESH => Text("Spiel kann beginnen")
      case Status.MY_MOVE => Text("Computer rechnet")
      case Status.YOUR_MOVE => Text("Du bist dran")
      case Status.REMIS => Text("Spiel endet unentschieden")
      case Status.AI_WON => Text("Du hast verloren")
      case Status.YOU_WON => Text("Du hast gewonnen")
    }
  }

}

