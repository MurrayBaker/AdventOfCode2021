import io.Source
import java.io.PrintWriter
import util.control.Exception.allCatch
import io.StdIn.readLine

def unknownPartMessage = "I don't know what you mean"

def day6: Unit = {
  println("Part 1 or 2?")
  val part = allCatch.opt(readLine().toInt);
  part match {
    case Some(p) => p match {
      case 1 => day6Part1
      case 2 => day6Part2
      case _ => println(unknownPartMessage)
    }
    case None => println(unknownPartMessage)
  }
}

val refractoryPeriod = 6
val ageOfConsent = 8
val numberOfIterations = 80
val scratchFilePath = "C:\\\\temp\\Fish.txt"

def day6Part1: Unit = {
  var initialState = Source.fromResource("day6input.txt").getLines().next();
  var nextIteration = initialState;
  var i = 0;
  for (i <- 1 to numberOfIterations) {
    println(i)
    nextIteration = timeMarchesInevitablyOnward(nextIteration);

    new PrintWriter(scratchFilePath) { 
      write(f"$nextIteration\r\n");
      close;
    }
  }

  val lastLine = Source.fromFile(scratchFilePath).getLines().foldLeft(Option.empty[String]) {
    case (_, line) => Some(line);
  };

  lastLine match {
    case Some(s) => println(s); println(s.split(",").count(s => true))
    case None => println("Done goofd");
  }
}

def timeMarchesInevitablyOnward(fishList:String) : String = 
  fishList.split(",")
  .map(n => {
    intParse(n) match{
      case Some(fishHorniness) => {
        if (fishHorniness == 0) 
          f"$refractoryPeriod,$ageOfConsent" 
          else (fishHorniness - 1).toString()
      }
      case None => ""
    }
  })
  .reduceLeft((a:String, b:String) => f"$a,$b");

def intParse(s: String): Option[Int] = 
  allCatch.opt(s.toInt);

def day6Part2: Unit = {
  var initialState = Source.fromResource("day6input.txt").getLines().next();

  var fishBrothel = scala.collection.mutable.Map[Int, BigInt]()

  (0 to 8).foreach(v => fishBrothel += (v, 0))
  
  initialState.split(",").foreach(s => intParse(s) match{
    case Some(n) => fishBrothel.updateWith(n)({
      case Some(x) => Some(x + 1)
      case None => Some(1)
    })
  })
  (1 to 256).foreach(i => {
    val ejaculatingFishCount = fishBrothel(0)
    (1 to 8).foreach(j => {
      fishBrothel.updateWith(j - 1)({
        case _ => Some(fishBrothel(j))
      })
    })
    fishBrothel.updateWith(ageOfConsent)({
      case _ => Some(ejaculatingFishCount)
    })
    fishBrothel.updateWith(refractoryPeriod)({
      case Some(n) => Some(n + ejaculatingFishCount)
      case None => Some(ejaculatingFishCount)
    })
  })

  println(fishBrothel.values.reduceLeft(_ + _))
}