package leetcode.fabura.ex91

import scala.annotation.tailrec

object DecodeWays extends App {

  println(numDecodings("100"))

  def numDecodings(s: String): Int = {

    @tailrec
    def inner(prev: Int, prevPrev: Int, idx: Int): Int = {
      if (idx < 0) prev
      else {
        val char = s.charAt(idx)
        char match {
          case '0' => inner(0, prev, idx - 1)
          case '1' => inner(prev + prevPrev, prev, idx - 1)
          case '2' if idx + 1 < s.length && s.charAt(idx + 1).toString.toInt <= 6 => inner(prev + prevPrev, prev, idx - 1)
          case _ => inner(prev, prev, idx - 1)
        }
      }
    }

    inner(1, 0, s.length - 1)
  }
}
