import util.control.Exception.allCatch
import io.StdIn.readLine

@main def main: Unit =
  while (true) {
    println("What day is it?")
    val day = allCatch.opt(readLine().toInt);
    day match {
      case Some(d) => d match {
        case 6 => day6
        case 7 => day7
        case 8 => day8
        case _ => println(unknownDayMessage)
      }
      case None => println(unknownDayMessage)
    }
  }
  
def unknownDayMessage = "That wasn't an integer day that we recognise."