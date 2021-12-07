import io.Source

def day7: Unit = {
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
