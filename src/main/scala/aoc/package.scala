import scala.annotation.tailrec
import scala.io.Source
import cats.effect.IO
import cats.effect.kernel.Resource

package object aoc {

  implicit class RegexHelper(val sc: StringContext) extends AnyVal {
    def re: scala.util.matching.Regex = sc.parts.mkString.r
  }

  def readFile1(fileName: String): Iterator[String] = Source.fromResource(fileName, this.getClass.getClassLoader).getLines()

//  def readFile2(fileName: String): Resource[IO, Source] =
 //   Resource.make(Source.fromResource(fileName, this.getClass.getClassLoader))(_.close())
    //Source.fromResource(fileName, this.getClass.getClassLoader).getLines()

  def readFile(fileName: String): Iterator[String] = Source.fromResource(fileName).getLines()
  //def readFile(fileName: String): Iterator[String] = Source.fromInputStream(getClass.getResourceAsStream(fileName)).getLines()

  @tailrec
  def iterate[T](v: T, f: T => T, p: T => Boolean): T = {
    if (p(v)) v else
      iterate(f(v), f, p)
  }

  @tailrec
  def iterateTimes[T](v: T, f: T => T, n: Int): T = {
    if (n == 0) v else
      iterateTimes(f(v), f, n - 1)
  }

  def md5(key: String, i: Int): Array[Byte] = {
    import java.security.MessageDigest
    val md = MessageDigest.getInstance("MD5")
    md.update((key + i.toString).getBytes)
    md.digest
    //    val str = BigInt(md.digest).toString(16)
    //    "0".repeat(32-str.length)+str
  }

  def hasFiveZeros(arr: Array[Byte]): Boolean = arr(0) == 0 && arr(1) == 0 && arr(2) < 16 && arr(2) > 0

  def hasSixZeros(arr: Array[Byte]): Boolean = arr(0) == 0 && arr(1) == 0 && arr(2) == 0

  object IntSeq extends Iterator[Int] {

    var cnt = 0

    override def hasNext: Boolean = true

    override def next(): Int = {
      cnt += 1
      cnt
    }

    def reset = cnt = 0
  }

  def or[T](f: T => Boolean, g: T => Boolean)(t: T) = f(t) || g(t)

  /**
   * Returns the neighboring cells in a 3x3 grid around a central element.  Handled edges and corners as well.
   *
   * @param xb
   * @param yb
   * @param x
   * @param y
   * @return
   */
  def neighbors(xb: Range, yb: Range)(x: Int, y: Int): Set[(Int, Int)] = {
    (for {
      i <- x - 1 to x + 1 if xb.contains(i)
      j <- y - 1 to y + 1 if yb.contains(j)
    } yield (i, j)).toSet - ((x, y))
  }

  /**
   * For use with neighbors above.  Removes cells diagonal from the center element
   *
   * @param x
   * @param y
   * @param p
   * @return
   */
  def filterDiagonals(x: Int, y: Int)(p: (Int, Int)): Boolean = p._1 == x || p._2 == y

  def manhattenDistance(x0: Int, y0: Int)(x: Int, y: Int) = {
    math.abs(x0 - x) + math.abs(y0 - y)
  }

  def cata[A, B](f: (A, B) => B, b: B, as: Seq[A]): B =
    if (as.isEmpty) b
    else f(as.head, cata(f, b, as.tail))

  def para[A, B](f: (A, (Seq[A], B)) => B, b: B, as: Seq[A]): B =
    if (as.isEmpty) b
    else f(as.head, (as.tail, para(f, b, as.tail)))

  def ana[A, B](f: B => (A, B), b: B): LazyList[A] =
    val (a, b1) = f(b)
    a +: ana(f, b1)

  extension[A: Ordering] (i: Iterable[A]) {
    def minMax: (A, A) = {
      import math.Ordering.Implicits.infixOrderingOps
      require(i.nonEmpty)
      i.foldLeft((i.head, i.head)) { case ((mn, mx), v) => (
        if (mn < v) mn else v, if (mx > v) mx else v)
      }
    }
  }
}
