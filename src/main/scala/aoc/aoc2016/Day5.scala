package aoc.aoc2016

object Day5 {

  import aoc._
  
  val doorId = "ojvtpuvg"
  
  def hexString(i: Int) = BigInt(md5(doorId, i)).toString(16)
  
  def hexString(arr: Array[Byte]) = {
    val str = BigInt(arr).toString(16)
    if (str.length < 27) "0"+str else str
  }
  
  def part1 = IntSeq.filter(i => hasFiveZeros(md5(doorId, i))).take(8).map(hexString).map(_.head).mkString //4543c154
  
  object Found {
    import scala.collection.mutable
    val set: mutable.Set[Int] = mutable.HashSet()
    
    def apply(e: Int) = {
      if (set(e)) true else {
        set.add(e)
        false
      }
    }
  }
  
  def password(s: Seq[String]): String = s.sorted.map(_.tail.head).mkString
  
  def part2 = {
    IntSeq.reset

    password(IntSeq.map(i => md5(doorId, i)).filter(or(hasFiveZeros, hasSixZeros)).
      filter(a => (0 to 7).contains(a(2)) && !Found(a(2))).map(hexString).take(8).toSeq) // 1050cbbd
  }
  
  
}
