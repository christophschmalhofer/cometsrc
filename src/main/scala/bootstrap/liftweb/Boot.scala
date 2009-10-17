package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {

    // where to search snippet
    LiftRules.addToPackages("comet")

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) :: Menu(Loc("Tic Tac Toe", List("tictactoe"), "Tic Tac Toe")) :: Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))


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
}

