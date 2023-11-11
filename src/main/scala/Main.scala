import scala.util.Random
import scala.compiletime.ops.boolean
import scala.annotation.tailrec

case class Point(x: Double, y: Double) {}

object Point {
  def createRandomPoint(generator: Random) = {
    Point(generator.nextDouble(), generator.nextDouble())
  }

}

def calculateDistanceTo0(point: Point): Double = {
  math.sqrt(math.pow(point.x, 2) + math.pow(point.y, 2))
}

def isPointInsideCircle(point: Point): Boolean = {
  calculateDistanceTo0(point) < 1
}

def calculatePi(generator: Random, N: Int): Double = {
  val numberOfPointsInsideCircle = (0 to N)
    .map(_i => {
      if isPointInsideCircle(Point.createRandomPoint(generator)) then 1 else 0
    })
    .reduce((acc, x) => acc + x)
  return numberOfPointsInsideCircle * 4.0 / N;
}

def calculatePiStart(generator: Random, N: Int): Double = {

  @tailrec
  def calculatePiRec(
      generator: Random,
      N: Int,
      numberOfPointsInsideCircle: Int
  ): Int = {
    if N <= 0 then {
      return numberOfPointsInsideCircle
    }
    val toAdd =
      if isPointInsideCircle(Point.createRandomPoint(generator)) then 1 else 0;

    val newNumberOfPointsInsideCircle = numberOfPointsInsideCircle + toAdd
    calculatePiRec(generator, N - 1, newNumberOfPointsInsideCircle)
  }

  calculatePiRec(generator, N, 0) * 4.0 / N;
}

def applyFunctionsToArg(lf: List[Int => Double], arg: Int): List[Double] = {

  @tailrec
  def applyRec(
      lf: List[Int => Double],
      arg: Int,
      acc: List[Double]
  ): List[Double] = {
    if lf.isEmpty then {
      return acc
    }
    val res = lf.head(arg)
    val tail = lf.tail
    val newAcc = acc.appended(res)
    applyRec(tail, arg, newAcc)

  }

  applyRec(lf, arg, List())
}

@main def hello: Unit = {
  // println("Hello world!")

  val generator = new Random();
  // val rand = generator.nextDouble();

  // val samplePoint = Point.createRandomPoint(generator);

  // println(s"sample point: ${samplePoint}");
  val N = 10000000
  val pi = calculatePi(generator, N);
  println(s"pi = ${pi}")

  val alsoPi = calculatePiStart(generator, N)
  println(s"alsoPI = ${alsoPi}")

  println("part 2 of challenge 1")
  val f0: Int => Double = x => x * 2.0
  val f1: Int => Double = x => x / 2.0
  val f2: Int => Double = x => x + 3.123;
  val f3: Int => Double = x => math.sqrt(x)
  val f4 = (x: Int) => x * 2.0
  val lf = List(f0, f1, f2, f3)
  val lf2 =
    List((x: Int) => x * 2.0, (x: Int) => x / 2.0, _ + 3.123, math.sqrt(_))

  val results = applyFunctionsToArg(lf, 2)
  println(s"results: $results")

  val results2 = applyFunctionsToArg(lf2, 2)
  println(s"results2: $results2")

}
