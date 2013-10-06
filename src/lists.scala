object lists extends App {
  val intList = List(1,2,3,4,5)
  val strList = List("a", "b", "c", "d")
  val shortList = List("a")


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


  def penultimate [T](l: List[T]): T = {
    var idx = l.length - 2

    if (idx < 0) idx = 0

    l(idx)
  }
  
  assert(penultimate(intList) == 4)
  assert(penultimate(strList) == "c")
  assert(penultimate(shortList) == "a")


  def nth [T](n: Int, l: List[T]): T = {
    if (n > l.length-1 || n < 0) throw new Error("n is out of bounds")
    l(n)
  }

  assert(nth(2, intList) == 3)
  assert(nth(0, strList) == "a")

  try {
    assert(nth(42, shortList) == "throw")
  } catch {
    case ex: Error => {
      println(ex)
    }
  }


  def nthRecursive [T](n: Int, l: List[T]): T = {
    def rec [T](count: Int, ls: List[T]): T = ls match {
      case i if count == 0 => i.head
      case _ :: el => rec(count - 1, ls.tail)
      case Nil => throw new Error("No such element.")
    }

    rec(n, l)
  }

  assert(nthRecursive(2, intList) == 3)
  assert(nthRecursive(0, strList) == "a")

  try {
    assert(nth(42, shortList) == "throw")
  } catch {
    case ex: Error => {
      println(ex)
    }
  }


  def length [T](l: List[T]): Int = {
    l.length
  }

  assert(length(intList) == 5)
  assert(length(strList) == 4)
  assert(length(shortList) == 1)


  def reverseBuiltIn [T](l: List[T]): List[T] = {
    l.reverse
  }

  assert(reverseBuiltIn(intList) == List(5,4,3,2,1))
  assert(reverseBuiltIn(strList) == List("d", "c", "b", "a"))
  assert(reverseBuiltIn(shortList) == List("a"))


  def reverseManual [T](l: List[T]): List[T] = {
    for (el <- l.reverse) yield el
  }

  assert(reverseManual(intList) == List(5,4,3,2,1))
  assert(reverseManual(strList) == List("d", "c", "b", "a"))
  assert(reverseManual(shortList) == List("a"))


  def isPalindrome [T](l: List[T]): Boolean = {
    l == l.reverse
  }

  assert(isPalindrome(List(1,2,3,2,1)))
  assert(!isPalindrome(intList))


  def flatten (l: List[_]): List[_] = l.flatMap {
    case list: List[_] => flatten(list)
    case other => List(other)
  }

  assert(flatten(List(1,2, List(3,4, List(5,6)))) == List(1,2,3,4,5,6))


//  def compressList (l: List[_]): List[_] = {
//    var last = l(0)
//
//
//  }
}