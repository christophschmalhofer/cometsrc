package comet.snippet

import _root_.net.liftweb.util.Log
import comet.model._
import comet.model.TicTacToe._

import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.js.JE._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.js.JsCmd


import _root_.net.liftweb.util.Log
import _root_.scala.xml._

class TicTacToeGame {

  def render = {

    // ticTacToe ist auch in der callback Closure
    val ticTacToe = new TicTacToe()

    var lineIndex = -1
    
    def renderLine(l:Array[Who]): Node = {
      lineIndex += 1 
      var columnIndex = -1

      def renderCell(cell:Who):Node = { 
        columnIndex += 1

        // das sind Closure Variablen für callback
        val currentColumn = columnIndex
        val currentLine = lineIndex

        def idStr() = currentLine.toString + currentColumn.toString

        // das ist die Ajax Callback Methode (enthält die Spiellogik)
        def callback():JsCmd =  {

          Log.info("In Zelle (" + currentLine + ", " + currentColumn +") gesetzt")

          //Spielfeld wird readonly angezeigt
          def displayResultBoard(winner:Option[PositionTriple]):JsCmd = {
            
            def partOfWinner(p:Position, winner:PositionTriple):Boolean = {
              for(i <- 0 to 2; if (winner.productElement(i).equals(p))) {
                return true
              }
              false
            }

            var cmds = Noop
            for( l <- 0 to 2; c <- 0 to 2) {
              val who = ticTacToe.getCellValue((l,c)).toString
              val renderWho = winner match {
                case Some(triple) => if(partOfWinner((l,c), triple)) <span style="background-color:red"> { who } </span> else Text(who)
                case None => Text(who)
              }
              cmds = cmds & SetHtml( l.toString + c.toString, renderWho) 
            } 
            cmds
          }
          
          try {
            ticTacToe.moveToPosition((currentLine, currentColumn), who.You)
            val myMove = ticTacToe.computeMove(who.Me)
            ticTacToe.checkGameOver()
            SetHtml( idStr(), Text(who.You.toString)) &
            SetHtml( myMove._1.toString + myMove._2.toString, Text(who.Me.toString)) &
            SetHtml( "moveFirst", Text(""))
          } catch {
            case ex: TicTacToe.GameOverException => { 
              Log.info( ex.player + " hat gewonnen")
              notice( ex.player + " hat gewonnen")
              displayResultBoard(ex.winnerTriple)}
          }
        }

        def renderWho(whoVal:Who):Node = {
          if (whoVal == who.Empty) <span> { EntityRef("nbsp") ++ EntityRef("nbsp") ++ EntityRef("nbsp")} </span> else Text(who.toString) 
        }

        // eine Zelle einer Zeile
        <td width="40" height="30">
        <span id={idStr()}>
        {
          a(callback _, renderWho(cell))
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
    // das gesamte Spielbrett
    <div>
    <span id="moveFirst">
     {
       ajaxButton( Text("Setze zuerst"), () => 
         { 
           val myMove = ticTacToe.computeMove(who.Me); 
           CmdPair(
             SetHtml( myMove._1.toString + myMove._2.toString, Text(who.Me.toString)),
             SetHtml( "moveFirst", Text("")))
         })
     }
    </span>
    <table border="1" style="font-size:300%;">
    { 
      ticTacToe.master.map( l => renderLine(l)) 
    }
    </table> 
    </div>
  }
}

