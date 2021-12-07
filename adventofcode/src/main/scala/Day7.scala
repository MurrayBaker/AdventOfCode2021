import io.Source
import util.control.Exception.allCatch
import io.StdIn.readLine

def unknownPartMessage = "I don't know what you mean"

def day7: Unit = {
    println("Part 1 or 2?")
    val part = allCatch.opt(readLine().toInt);
    part match {
        case Some(p) => p match {
            case 1 => day7Part1
            case 2 => day7Part2
            case _ => println(unknownPartMessage)
        }
        case None => println(unknownPartMessage)
    }
}

def day7Part1: Unit = {
    val startingState = Source.fromResource("day7input.txt").getLines().next();

    val crabs = startingState
        .split(",")
        .map(crab => {
            intParse(crab) match {
                case Some(i) => i
                case None => 0
            }
        })

    val fuelValues = (0 to crabs.max).map(i => {
         calculateFuelRequired(crabs, i)
    })

    println(fuelValues.min)
}

def calculateFuelRequired(crabs : Array[Int], position : Int): Int = 
    crabs.map(c => Math.abs(c - position)).sum

def calculateFuelRequiredHarmonic(crabs : Array[Int], position : Int): Int = 
    crabs.map(c => getTriangularNumber(Math.abs(c - position))).sum

def getTriangularNumber(input : Int): Int = 
    (input * (input + 1)) / 2

def day7Part2: Unit = {
    val startingState = Source.fromResource("day7input.txt").getLines().next();

    val crabs = startingState
        .split(",")
        .map(crab => {
            intParse(crab) match {
                case Some(i) => i
                case None => 0
            }
        })

    val fuelValues = (0 to crabs.max).map(i => {
         calculateFuelRequiredHarmonic(crabs, i)
    })

    println(fuelValues.min)
}