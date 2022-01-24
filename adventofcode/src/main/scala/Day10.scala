import io.Source
import util.control.Exception.allCatch
import io.StdIn.readLine
import collection.mutable.Stack

def day10: Unit = {
    println("Part 1 or 2?")
    val part = allCatch.opt(readLine().toInt);
    part match {
        case Some(p) => p match {
            case 1 => day10Part1
            case 2 => day10Part2
            case _ => println(unknownPartMessage)
        }
        case None => println(unknownPartMessage)
    }
}

def day10Part1 : Unit = 
    println(Source.fromResource("day10input.txt")
        .getLines()
        .map(line => maybeGetInvalidSymbol(line))
        .map(invalidSymbol => invalidSymbol match {
            case Some(a) => a match { 
                case ')' => 3
                case ']' => 57
                case '}' => 1197
                case '>' => 25137
            }
            case None => 0
        })
        .sum)

def maybeGetInvalidSymbol(line : String) : Option[Char] = 
    val stack = new Stack[Char]()
    line.foreach(symbol => {
        symbol match {
            case '(' => stack.push('(')
            case '[' => stack.push('[')
            case '{' => stack.push('{')
            case '<' => stack.push('<')
            case ')' => if stack.top == '(' then stack.pop() else return Some(')')
            case ']' => if stack.top == '[' then stack.pop() else return Some(']')
            case '}' => if stack.top == '{' then stack.pop() else return Some('}')
            case '>' => if stack.top == '<' then stack.pop() else return Some('>')
        }
    })

    return None

def day10Part2 : Unit = 
    println(oddMedian(Source.fromResource("day10input.txt")
        .getLines()
        .filter(line => maybeGetInvalidSymbol(line) == None)
        .map(line => getClosingCharacters(line)
            .map(character => character match {
                case ')' => 1
                case ']' => 2
                case '}' => 3
                case '>' => 4
            })
            .foldLeft(0 : BigInt) { (a,b) => a * 5 + b })
        .toSeq))


def getClosingCharacters(input : String) : String = 
    val stack = new Stack[Char]()
    input.foreach(symbol => {
        symbol match {
            case '(' => stack.push(')')
            case '[' => stack.push(']')
            case '{' => stack.push('}')
            case '<' => stack.push('>')
            case ')' | ']' | '}' | '>' => stack.pop()
        }
    })

    return stack.popAll.reverse.mkString

def oddMedian(input : Seq[BigInt]) : BigInt = 
    input.sortWith(_<_).drop(input.length / 2).head