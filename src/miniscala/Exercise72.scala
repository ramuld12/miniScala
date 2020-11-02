package miniscala


object Exercise72 {

  def main(args: Array[String]): Unit = {
    assert(!orElse(a = false, b = false))
    assert(orElse(a = true, b = true))
    assert(orElse(a = false, b = true))
    assert(orElse(a = true, b = false))

    var x = 0
    assert(orElse(true, {
      x += 1; false
    })) // only first argument should be evaluated
    assert(x == 0)
    assert(orElse(false, {
      x += 1; true
    })) // second argument should be evaluated (once)
    assert(x == 1)
  }

  def orElse(a: => Boolean, b: => Boolean): Boolean = {
    if (a) true
    else {
      if (b) true
      else false
    }
  }
}
