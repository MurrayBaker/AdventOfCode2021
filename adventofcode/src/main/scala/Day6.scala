import io.Source
import java.io.PrintWriter
import util.control.Exception.allCatch

def refractoryPeriod = 6
def ageOfConsent = 8
def numberOfIterations = 80
def scratchFilePath = "C:\\\\temp\\Fish.txt"

def day6: Unit = {
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