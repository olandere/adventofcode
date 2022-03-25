package aoc

import scala.annotation.tailrec

package object aoc2020 {

  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  def lcm(a: BigInt, b: BigInt): BigInt = a * b / gcd(a, b)

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    println(s"a: $a, b: $b")
    if (b == 0) a else if (a < b) gcd(a, b % a)
    else {
      gcd(b, a % b)
    }
  }

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = {
    println(s"a: $a, b: $b")
    if (b == 0) a else if (a < b) gcd(a, b % a)
    else {
      gcd(b, a % b)
    }
  }

  def gcd(as: Seq[Int]): Int = as.reduce(gcd)

  @tailrec
  def euclid(a: Int, b: Int, i: Int = 1): Int = {
    if (b == 1) a else if (a < b) {
      euclid(b, a, i)
    }
    else {
      val r1 = a
      val r2 = b
      val r3 = a % b
      val q = a / b
      println(s"$r3 = $r1 - $q · $r2")
      println(s"r$i($a) = r${i + 1}($b) · q$i(${a / b}) + r${i + 2}(${a % b})")
      euclid(b, a % b, i + 1)
    }
  }

  def quotients(a: Int, b: Int): Seq[Int] = {
    @tailrec
    def helper(a: Int, b: Int, acc: Seq[Int]): Seq[Int] = {
      if (b == 0) acc else if (a < b) helper(a, b % a, acc :+ b / a)
      else {
        helper(b, a % b, acc :+ a / b)
      }
    }

    helper(a, b, Seq())
  }

  @tailrec
  def ks(k0: Int, k1: Int, qs: Seq[Int]): Int = {
    println(s"$k0, $k1")
    if (qs.isEmpty) k0 else ks(k1, k0 - qs.head * k1, qs.tail)
  }

  /**
   * ax + by = (a, b)
   *
   * @param a
   * @param b
   * @return
   */
  def solve(a: Int, b: Int): (Int, Int) = {
    val qs = quotients(a, b)
    val k = ks(1, 0, qs)
    val m = ks(0, 1, qs)
    println(k * a + m * b)
    require(k * a + m * b == gcd(a, b))
    (k, m)
  }

  /**
   * Solve for x where ax ≡ b (mod n)
   */
  def solveX(a: BigInt, b: BigInt, n: BigInt): BigInt = {
    println(s"${a}x ≡ $b (mod $n)")
    if (a == 1) b else if (a == b) 1 else
      if (n < 11) (1 to n.intValue).find{i => residue(a*i, n) == b}.get else {
        val y = solveX(residue(n, a), residue(-b, a), a)
        println(s"y = $y")
        (n * y + b) / a
      }
  }

  /**
   * Is a ≡ b (mod n)?
   *
   * @param a
   * @param b
   * @param n
   * @return
   */
  def isCongruent(a: Int, b: Int, n: Int): Boolean = (a - b) % n == 0


  /**
   * Is a ≡ b (mod n)?
   *
   * @param a
   * @param b
   * @param n
   * @return
   */
  def isCongruent(a: BigInt, b: BigInt, n: BigInt): Boolean = (a - b) % n == 0

  /**
   * residue(a, n) ≡ a (mod n)
   *
   * @param a
   * @param n
   * @return
   */
  def residue(a: Int, n: Int): Int = if (a < 0) //residue(a + n, n) else a % n
    (a - n * (a / n) + n) % n else a % n

  /**
   * residue(a, n) ≡ a (mod n)
   *
   * @param a
   * @param n
   * @return
   */
  def residue(a: BigInt, n: BigInt): BigInt = if (a < 0) //residue(a + n, n) else a % n
    (a - n * (a / n) + n) % n else a % n

  /**
   * a ≡ b (mod (m, n))
   *
   * @param a
   * @param m
   * @param b
   * @param n
   * @return
   */
  def isSovable(a: Int, m: Int, b: Int, n: Int): Boolean =
    isCongruent(a, b, gcd(m, n))


  /**
   * a ≡ b (mod (m, n))
   *
   * @param a
   * @param m
   * @param b
   * @param n
   * @return
   */
  def isSovable(a: BigInt, m: BigInt, b: BigInt, n: BigInt): Boolean =
    isCongruent(a, b, gcd(m, n))

  /**
   * Solves for x when x ≡ a (mod m) and x ≡ b (mod n)
   * @param a
   * @param m
   * @param b
   * @param n
   * @return
   */
  def solveX(a: Int, m: Int, b: Int, n: Int): Option[(Int, Int)] = {
    if (isSovable(a, m, b, n)) {
      val t = solveX(residue(m, n), residue(b-a, n), n)
      val nm = lcm(m, n)
      Some(((a + m * t) % nm).intValue, nm)
    } else None
  }


  /**
   * Solves for x when x ≡ a (mod m) and x ≡ b (mod n)
   * @param a
   * @param m
   * @param b
   * @param n
   * @return
   */
  def solveX(a: BigInt, m: BigInt, b: BigInt, n: BigInt): Option[(BigInt, BigInt)] = {
    if (isSovable(a, m, b, n)) {
      val t = solveX(residue(m, n), residue(b-a, n), n)
      val nm = lcm(m, n)
      Some((a + m * t) % nm, nm)
    } else None
  }

  def extendedEuclid(x: Int, y: Int): (Int, Int, Int) = {
    @tailrec
    def helper(a: Int, b: Int, g: Int, u: Int, v: Int, w: Int): (Int, Int, Int) = {
      println(s"$a, $b, $g, $u, $v, $w")
      if (w > 0) {
        val q = g/w
        helper(u, v, w, a-q*u, b-q*v, g-q*w)
      } else (a, b, g)
    }
    helper(1, 0, x, 0, 1, y)
  }
}
