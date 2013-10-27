object lists extends App {
  val intList = List(1,2,3,4,5)
  val strList = List("a", "b", "c", "d")
  val shortList = List("a")


  // p01
  def lastBuiltIn [T](l: List[T]): T = {
    l.last
  }

  assert(lastBuiltIn(intList) == 5)
  assert(lastBuiltIn(strList) == "d")
  assert(lastBuiltIn(shortList) == "a")


  def lastManual [T](l: List[T]): T = {
    var idx = l.length - 1
    
    if (idx < 0) idx = 0
    
    l(idx)
  }

  assert(lastManual(intList) == 5)
  assert(lastManual(strList) == "d")


  //p02
  def penultimate [T](l: List[T]): T = {
    var idx = l.length - 2

    if (idx < 0) idx = 0

    l(idx)
  }
  
  assert(penultimate(intList) == 4)
  assert(penultimate(strList) == "c")
  assert(penultimate(shortList) == "a")


  // p03
  def nth [T](n: Int, l: List[T]): T = {
    if (n > l.length-1 || n < 0) throw new Error("n is out of bounds")
    l(n)
  }

  assert(nth(2, intList) == 3)
  assert(nth(0, strList) == "a")

  try {
    assert(nth(42, shortList) == "throw")
  } catch {
    case ex: Error => {}
  }


  def nthRecursive [T](n: Int, l: List[T]): T = {
    def rec (count: Int, ls: List[T]): T = (count, ls) match {
      case (0, h :: _) => h
      case (c, _ :: tail) => rec(count - 1, tail)
      case (_, Nil) => throw new Error("No such element.")
    }

    rec(n, l)
  }

  assert(nthRecursive(2, intList) == 3)
  assert(nthRecursive(0, strList) == "a")

  try {
    assert(nthRecursive(42, shortList) == "throw")
  } catch {
    case ex: Error => {}
  }


  // p04
  def length [T](l: List[T]): Int = {
    l.length
  }

  assert(length(intList) == 5)
  assert(length(strList) == 4)
  assert(length(shortList) == 1)


  def lengthRecursive [T](l: List[T]): Int = {
    def len(ls: List[T]): Int = ls match {
      case Nil => 0
      case _ => 1 + len(ls.tail)
    }

    len(l)
  }

  assert(lengthRecursive(intList) == 5)
  assert(lengthRecursive(strList) == 4)
  assert(lengthRecursive(shortList) == 1)


  def lengthTailRecursive [T](l: List[T]): Int = {
    def len(acc: Int, ls: List[T]): Int = ls match {
      case Nil => acc
      case _ :: tail => len(acc + 1, tail)
    }

    len(0, l)
  }

  assert(lengthTailRecursive(intList) == 5)
  assert(lengthTailRecursive(strList) == 4)
  assert(lengthTailRecursive(shortList) == 1)


  // p05
  def reverseBuiltIn [T](l: List[T]): List[T] = {
    l.reverse
  }

  assert(reverseBuiltIn(intList) == List(5,4,3,2,1))
  assert(reverseBuiltIn(strList) == List("d", "c", "b", "a"))
  assert(reverseBuiltIn(shortList) == List("a"))


  def reverseRecursive (l: List[_]): List[_] = {
    def reverseRec (ls: List[_]): List[_] = ls match {
      case Nil => Nil
      case h :: tail => reverseRecursive(tail) ::: List(h)
    }

    reverseRec(l)
  }

  assert(reverseRecursive(intList) == List(5,4,3,2,1))
  assert(reverseRecursive(strList) == List("d", "c", "b", "a"))
  assert(reverseRecursive(shortList) == List("a"))


  def reverseTailRecursive [_](l: List[_]): List[_] = {
    def reverseRec (reversed: List[_], current: List[_]): List[_] = current match {
      case Nil => reversed
      case h :: tail => reverseRec(h :: reversed, tail)
    }

    reverseRec(Nil, l)
  }

  assert(reverseTailRecursive(intList) == List(5,4,3,2,1))
  assert(reverseTailRecursive(strList) == List("d", "c", "b", "a"))
  assert(reverseTailRecursive(shortList) == List("a"))


  // p06
  def isPalindrome [T](l: List[T]): Boolean = {
    l == l.reverse
  }

  assert(isPalindrome(List(1,2,3,2,1)))
  assert(!isPalindrome(intList))


  // p07
  def flatten (l: List[_]): List[_] = {
    def flat (l: List[_]): List[_] = l.flatMap {
      case list: List[_] => flat(list)
      case other => List(other)
    }

    flat(l)
  }

  assert(flatten(List(1,2, List(3,4, List(5,6)))) == List(1,2,3,4,5,6))


  // p08
  def compressList (l: List[_]): List[_] = {
    def compress (result: List[_], ls: List[_]): List[_] = ls match {
      case Nil => result
      case h :: tail => {
        if (result.isEmpty || result.last != h) compress(result :+ h, tail)
        else compress(result, tail)
      }
    }

    compress(Nil, l)
  }

  assert( compressList(List("a", "a", "b", "a", "c", "c", "c", "b")) == List("a", "b", "a", "c", "b") )


  // p09
  def compressListIntoLists (l: List[_]): List[_] = {
    def compress (result: List[_], ls: List[_]): List[_] = ls match {
      case Nil => result
      case h :: tail => compress(result :+ ls.takeWhile(_ == h), ls.dropWhile(_ == h))
    }

    compress(Nil, l)
  }

  assert(
    compressListIntoLists(List("a", "a", "b", "a", "c", "c", "c", "b")) ==
                 List(List("a", "a"), List("b"), List("a"), List("c", "c", "c"), List("b"))
  )


  // p10
  def RLE (l: List[_]): List[_] = {
    def compress (result: List[_], ls: List[_]): List[_] = ls match {
      case Nil => result
      case h :: tail => {
        val (span, tail) = ls span (_ == h)
        compress(result :+ (span.length, h), tail)
      }
    }

    compress(Nil, l)
  }

  assert(
    RLE(List("a", "a", "b", "c", "c", "c")) ==
    List((2,"a"), (1,"b"), (3,"c"))
  )

  // p11
  def RLEmodified (l: List[_]): List[_] = {
    def compress (result: List[_], ls: List[_]): List[_] = ls match {
      case Nil => result
      case h :: tail => {
        val (span, tail) = ls span (_ == h)

        if (span.length == 1) compress(result :+ h, tail)
        else compress(result :+ (span.length, h), tail)
      }
    }

    compress(Nil, l)
  }

  assert(
    RLEmodified(List("a", "a", "b", "c", "c", "c", "d")) ==
      List((2,"a"), "b", (3,"c"), "d")
  )
}