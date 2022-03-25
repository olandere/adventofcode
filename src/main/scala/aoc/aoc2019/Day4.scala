package aoc.aoc2019

object Day4 {

  def isValid(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int) : Boolean = {
    val i = s"$a$b$c$d$e$f".toInt
    a <= b && b <= c && c <= d && d <= e && e <= f && (
      a == b || b == c || c == d || d == e || e == f) &&
      i >= 134564 && i <= 585159
  }

  def isValid2(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int) : Boolean = {
    //def onlyDubble(a: Int, b: Int, c: Int) = a == b ^ b == c
    def onlyDubble(i: Seq[Int]) = {
      i.exists{n => i.count(_ == n) == 2} || i.forall{n => i.count(_ == n) == 1}
    }

    isValid(a, b, c, d, e, f) && onlyDubble(Seq(a,b,c,d,e,f))
     // onlyDubble(a, b, c) || onlyDubble(b, c, d) || onlyDubble(c, d, e) || onlyDubble(d, e, f) )
  }

  def part1 = {
    (for {
      a <- 1 to 5
      b <- 1 to 9
      c <- 1 to 9
      d <- 1 to 9
      e <- 1 to 9
      f <- 1 to 9 if isValid(a, b, c, d, e, f)
    } yield (a,b,c,d,e,f)).size // 1929
  }

  def part2 = {
    (for {
      a <- 1 to 5
      b <- 1 to 9
      c <- 1 to 9
      d <- 1 to 9
      e <- 1 to 9
      f <- 1 to 9 if isValid2(a, b, c, d, e, f)
    } yield (a,b,c,d,e,f)).size // 1306
  }

}
