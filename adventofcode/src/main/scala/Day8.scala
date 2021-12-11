import io.Source
import util.control.Exception.allCatch
import io.StdIn.readLine

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

def day8Part2: Unit = 
    println(Source.fromResource("day8input.txt")
    .getLines()
    .map(line => line
        .split(" ")
        .drop(11)
        .map(digit => digit
            .map(character => line
                .split(" ")
                .take(10)
                .mkString
                .groupBy(_.toChar)
                .map(x => (x._1, x._2.length) match {
                    case (y, 4) => (y, "e")
                    case (y, 6) => (y, "b")
                    case (y, 9) => (y, "f")
                    case (y, 8) => if line.split(" ").take(10).filter(l => l.length == 2)(0).contains(y) 
                        then (y, "c") else (y, "a")
                    case (y, 7) => if line.split(" ").take(10).filter(l => l.length == 4)(0).contains(y) 
                        then (y, "d") else (y, "g")
                })(character))
                .mkString
                .sortBy(identity) match {
                    case "abcefg" => "0"
                    case "cf" => "1"
                    case "acdeg" => "2"
                    case "acdfg" => "3"
                    case "bcdf" => "4"
                    case "abdfg" => "5"
                    case "abdefg" => "6"
                    case "acf" => "7"
                    case "abcdefg" => "8"
                    case "abcdfg" => "9"
                }).mkString.toInt).sum)