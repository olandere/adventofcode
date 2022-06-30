package aoc.aoc2021

import scala.annotation.tailrec

object Day16 {

  val input = "2052ED9802D3B9F465E9AE6003E52B8DEE3AF97CA38100957401A88803D05A25C1E00043E1545883B397259385B47E40257CCEDC7401700043E3F42A8AE0008741E8831EC8020099459D40994E996C8F4801CDC3395039CB60E24B583193DD75D299E95ADB3D3004E5FB941A004AE4E69128D240130D80252E6B27991EC8AD90020F22DF2A8F32EA200AC748CAA0064F6EEEA000B948DFBED7FA4660084BCCEAC01000042E37C3E8BA0008446D8751E0C014A0036E69E226C9FFDE2020016A3B454200CBAC01399BEE299337DC52A7E2C2600BF802B274C8848FA02F331D563B3D300566107C0109B4198B5E888200E90021115E31C5120043A31C3E85E400874428D30AA0E3804D32D32EED236459DC6AC86600E4F3B4AAA4C2A10050336373ED536553855301A600B6802B2B994516469EE45467968C016D004E6E9EE7CE656B6D34491D8018E6805E3B01620C053080136CA0060801C6004A801880360300C226007B8018E0073801A801938004E2400E01801E800434FA790097F39E5FB004A5B3CF47F7ED5965B3CF47F7ED59D401694DEB57F7382D3F6A908005ED253B3449CE9E0399649EB19A005E5398E9142396BD1CA56DFB25C8C65A0930056613FC0141006626C5586E200DC26837080C0169D5DC00D5C40188730D616000215192094311007A5E87B26B12FCD5E5087A896402978002111960DC1E0004363942F8880008741A8E10EE4E778FA2F723A2F60089E4F1FE2E4C5B29B0318005982E600AD802F26672368CB1EC044C2E380552229399D93C9D6A813B98D04272D94440093E2CCCFF158B2CCFE8E24017CE002AD2940294A00CD5638726004066362F1B0C0109311F00424CFE4CF4C016C004AE70CA632A33D2513004F003339A86739F5BAD5350CE73EB75A24DD22280055F34A30EA59FE15CC62F9500"

  val hexToBin: Map[Char, String] = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  sealed trait Packet {
    val version: Int
    val typeId: Int
    val length: Long
  }

  case class Literal(version: Int, typeId: Int, value: BigInt, length: Long) extends Packet

  case class Operator(version: Int, typeId: Int, length: Long, subpackets: Seq[Packet]) extends Packet

  def parseLiteral(p: String): (BigInt, Int, String) = {
    @tailrec
    def helper(bits: Seq[String], len: Int, acc: String): (BigInt, Int, String) = {
      if (bits.head(0) == '0') {
        (binToInt(acc + bits.head.tail), len + 5, bits.tail.mkString)
      } else {
        helper(bits.tail, len + 5, acc + bits.head.tail)
      }
    }

    helper(p.grouped(5).toSeq, 0, "")
  }

  def parsePacket(p: String): (String, Packet) = {

    def parseOperator0(version: Int, typeId: Int, payload: String) = {

      @tailrec
      def subPackets(p: String, len: Long, acc: Seq[Packet]): (String, Seq[Packet]) = {
//        println(s"len = $len")
        if (len <= 0 || p.length < 8)
//          println(s"len = $len")
          (p, acc)
        else {
          val (r, pkt) = parsePacket(p)
          subPackets(r, len - pkt.length, acc :+ pkt)
        }
      }

      val subPacketLen = binToInt(payload.tail.take(15)).intValue
      val (rest, pkts) = subPackets(payload.drop(16), subPacketLen, Seq())
      (rest, Operator(version, typeId, subPacketLen, pkts))
    }

    def parseOperator1(version: Int, typeId: Int, payload: String) = {
      @tailrec
      def subPackets(p: String, len: Int, acc: Seq[Packet]): (String, Seq[Packet]) = {
        if (len <= 0 || p.length < 8)
          (p, acc)
        else {
          val (r, pkt) = parsePacket(p)
          subPackets(r, len - 1, acc :+ pkt)
        }
      }

      val subPacketNum = binToInt(payload.tail.take(11)).intValue
      val (rest, pkts) = subPackets(payload.drop(12), subPacketNum, Seq())
      (rest, Operator(version, typeId, subPacketNum, pkts))
    }

//    println(p)
    val (version, typeId, payload) = (binToInt(p.take(3)).intValue, binToInt(p.slice(3, 6)).intValue, p.drop(6))
    typeId match {
      case 4 => {
        val res: (BigInt, Int, String) = parseLiteral(payload)
        //helper(res._3, acc :+ Literal(version, typeId, res._1, res._2+6))
        (res._3, Literal(version, typeId, res._1, res._2 + 6))
      }
      case _ => payload.head match {
        case '0' =>
          parseOperator0(version, typeId, payload)
        // (binToInt(payload.tail.take(15)), payload.drop(16))
        // helper(payload.drop(16), acc)
        // Operator(version, typeId, 0, helper(payload.drop(16), acc))
        case '1' =>
          parseOperator1(version, typeId, payload)
        //   (binToInt(payload.tail.take(11)), payload.drop(12))
        //   helper(payload.drop(12), acc)
      }
    }
  }

  def binToInt(s: String): BigInt = BigInt(s, 2)

  def versionSum(p: Packet): Int = {
    p match
      case l: Literal => l.version
      case o: Operator => o.version + o.subpackets.map(versionSum).sum
  }

  def eval(p: Packet): BigInt = {
    p match
      case l: Literal => l.value
      case o: Operator => o.typeId match
        case 0 => o.subpackets.map(eval).sum
        case 1 => o.subpackets.map(eval).product
        case 2 => o.subpackets.map(eval).min
        case 3 => o.subpackets.map(eval).max
        case 5 =>
          val r = o.subpackets.map(eval)
          if (r(0) > r(1)) 1 else 0
        case 6 =>
          val r = o.subpackets.map(eval)
          if (r(0) < r(1)) 1 else 0
        case 7 =>
          val r = o.subpackets.map(eval)
          if (r(0) == r(1)) 1 else 0
  }

  def printPkt(depth: Int = 0)(p: Packet): Unit  = {
    p match
      case l: Literal => print(s"${l.value} ")
      case o: Operator =>
        val s = "  "*depth
        println()
        print(s)
        o.typeId match
        case 0 => print("sum ("); o.subpackets.map(printPkt(depth+1)); print(")")
        case 1 => print("product ("); o.subpackets.map(printPkt(depth+1)); print(")")
        case 2 => print("min ("); o.subpackets.map(printPkt(depth+1)); print(")")
        case 3 => print("max ("); o.subpackets.map(printPkt(depth+1)); print(")")
        case 5 =>
          print("<")
          require(o.subpackets.length == 2,s"Got ${o.subpackets.length}")
          o.subpackets.map(printPkt(depth+1))
        case 6 =>
          print(">")
          require(o.subpackets.length == 2)
          o.subpackets.map(printPkt(depth+1))
        case 7 =>
          print("=")
          require(o.subpackets.length == 2)
          o.subpackets.map(printPkt(depth+1))
  }


  def part1(): Int = {
    def test(s: String) = {
      val (r, p) = parsePacket(s.map(hexToBin).mkString)
      println(s"$s: $p ${versionSum(p)} $r")
    }

//    test("D2FE28")
//    test("38006F45291200")
//    test("EE00D40C823060")
//    test("8A004A801A8002F478")
//    test("620080001611562C8802118E34")
//    test("C0015000016115A2E0802F182340")
//    test("A0016C880162017C3686B18A3D4780")

    versionSum(parsePacket(input.map(hexToBin).mkString)._2) // 957
  }

  def part2(): BigInt = {
    def test(s: String, expected: Int) = {
      val (_, p) = parsePacket(s.map(hexToBin).mkString)
      val r = eval(p)
      printPkt(0)(p)
      println(s"$s: $r")
      require(r == expected)
    }

    test("D2FE28", 2021)
    test("C200B40A82", 3)
    test("04005AC33890", 54)
    test("880086C3E88112", 7)
    test("CE00C43D881120", 9)
    test("D8005AC2A8F0", 1)
    test("F600BC2D8F", 0)
    test("9C005AC2F8F0", 0)
    test("9C0141080250320F1802104A08", 1)
    def pkt = parsePacket(input.map(hexToBin).mkString)._2
    printPkt(0)(pkt)
    eval(pkt)
  }
}
