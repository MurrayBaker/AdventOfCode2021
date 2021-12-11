import io.Source
import util.control.Exception.allCatch
import io.StdIn.readLine

def day9: Unit = {
    println("Part 1 or 2?")
    val part = allCatch.opt(readLine().toInt);
    part match {
        case Some(p) => p match {
            case 1 => day9Part1
            case 2 => day9Part2
            case _ => println(unknownPartMessage)
        }
        case None => println(unknownPartMessage)
    }
}

def day9Part1: Unit = {
    val lines = Source.fromResource("day9input.txt").getLines()
    
    var previousLine = Option.empty[String]
    var nextLine = Option.empty[String]
    var currentLine = lines.next
    var total = 0
    while (currentLine != ""){
        nextLine = if lines.hasNext then Some(lines.next) else None

        for (i <- 0 to currentLine.length - 1) {
            val above = previousLine match {
                case Some(a) => a.charAt(i).asDigit
                case None => 10
            }
            val below = nextLine match {
                case Some(a) => a.charAt(i).asDigit
                case None => 10
            }
            var left = if i == 0 then 10 else currentLine.charAt(i - 1).asDigit
            var right = if i == currentLine.length - 1 then 10 else currentLine.charAt(i + 1).asDigit

            var current = currentLine.charAt(i).asDigit
            if (current < above && current < below && current < left && current < right) {
                total += (current + 1)
            }
        }

        previousLine = Some(currentLine)

        currentLine = nextLine match {
            case Some(s) => s
            case None => ""
        }
    }

    println(total)
}
    
def day9Part2: Unit = 
    println("Todo")