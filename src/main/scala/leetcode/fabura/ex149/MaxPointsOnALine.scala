package leetcode.fabura.ex149

case class Point(x: Int, y: Int)

object Solution {

  class Fraction(x: Int, y: Int) {
    private val g = gcd(x, y)
    private val _x = x / g
    private val _y = y / g

    private def gcd(a: Int, b: Int): Int =
      if (a == 0 && b == 0) 1 else if (b == 0) a else gcd(b, a % b)

    def canEqual(other: Any): Boolean = other.isInstanceOf[Fraction]

    override def equals(other: Any): Boolean = other match {
      case that: Fraction =>
        (that canEqual this) &&
          _x == that._x &&
          _y == that._y
      case _ => false
    }

    override def hashCode(): Int = (_x -> _y).hashCode()
  }

  object Fraction {
    def apply(x: Int, y: Int): Fraction = new Fraction(x, y)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Point(0, 0), Point(1, 1), Point(0, 0))

    println(maxPoints(arr))
  }

  def maxPoints(points: Array[Point]): Int = {

    if (points.length <= 1) {
      points.length
    } else {

      val pointsSeq = points.toIndexedSeq
      val zeroFraction = Fraction(0, 0)

      val linesSizes = for (i <- points.indices) yield {
        val thisPoint = pointsSeq(i)
        val decimalToPoints = pointsSeq.drop(i).groupBy(p => Fraction(p.x - thisPoint.x, p.y - thisPoint.y))
        val countOfSamePoints = decimalToPoints.get(zeroFraction).map(_.length).getOrElse(0)
        val notTheSamePointsMap = decimalToPoints - zeroFraction
        if (notTheSamePointsMap.nonEmpty) notTheSamePointsMap.values.maxBy(_.length).length + countOfSamePoints else countOfSamePoints
      }

      linesSizes.max
    }
  }
}
