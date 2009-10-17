package comet.snippet
import _root_.net.liftweb.util.Log

class HelloWorld {
  def howdy = { Log.info("howdy called"); <span>Welcome to cometx at {new _root_.java.util.Date}</span>}
}

