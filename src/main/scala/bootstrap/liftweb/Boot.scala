package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import scala.actors._ 
import TimeHelpers.intToTimeSpanBuilder
import TimeHelpers.timeSpanToLong 


object RequestLogger {

  def log(req: Box[Req], time: Long, queries: List[(String, Long)]): Unit = {
    Log.info("RA Request "+req.map(_.uri).openOr("No Request")+ " took "+time)
  }
}

object SessionInfoDumper extends Actor {
    def act() {
      while (true) {
        receive {
          case info:Any => println( "SessionInfoDumper: " + info)
        }
      }
    }
}

class Boot {

  def boot {

    // where to search snippet
    LiftRules.addToPackages("comet")

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) :: Menu(Loc("Tic Tac Toe", List("tictactoe"), "Tic Tac Toe")) :: Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))


    //S.addAnalyzer(RequestLogger.log _)
    
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)
    

    SessionMaster.sessionWatchers = SessionInfoDumper :: SessionMaster.sessionWatchers
  } 

  LogBoot.defaultProps =  
    """<?xml version="1.0" encoding="UTF-8" ?>  
  <!DOCTYPE log4j:configuration SYSTEM "log4j.dtd">    
  <log4j:configuration xmlns:log4j="http://jakarta.apache.org/log4j/">    
  <appender name="appender" class="org.apache.log4j.ConsoleAppender">    
  <layout class="org.apache.log4j.SimpleLayout"/>    
  </appender>    
  <root>    
  <priority value="ALL"/>    
  <appender-ref ref="appender"/>    
  </root>    
  </log4j:configuration>    
  """ 

}


