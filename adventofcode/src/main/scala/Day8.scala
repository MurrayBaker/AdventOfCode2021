import io.Source
import util.control.Exception.allCatch
import io.StdIn.readLine

def unknownPartMessage = "I don't know what you mean"

def day8: Unit = {
    println("Part 1 or 2?")
    val part = allCatch.opt(readLine().toInt);
    part match {
        case Some(p) => p match {
            case 1 => day8Part1
            case 2 => day8Part2
            case _ => println(unknownPartMessage)
        }
        case None => println(unknownPartMessage)
    }
}

def day8Part1: Unit =
    println(Source.fromResource("day8input.txt").getLines()
        .map(line => line
            .split(" ")
            .drop(line.split(" ").indexOf("|"))
            .filter(v => List(2,3,4,7).contains(v.length))
            .count(_ => true))
            .sum)

def day8Part2: Unit = {

}