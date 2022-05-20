object MyZip {
  def myZip(s1: Seq[Any], s2: Seq[Any]): Seq[Any] = {

    if (s1.isEmpty && s2.isEmpty) {
      Seq()
    }

    else if (s1.length > s2.length) {
      val lengthDifference = s1.length - s2.length
      val eq1 = s1.dropRight(lengthDifference)
      val y1 = eq1.zip(s2).flatMap(_.productIterator)
      val y2 = s1.takeRight(lengthDifference)
      y1 ++ y2
    }

    else if (s2.length > s1.length) {
      val lengthDifference2 = s2.length - s1.length
      val eq2 = s2.dropRight(lengthDifference2)
      val x1 = s1.zip(eq2).flatMap(_.productIterator)
      val x2 = s2.takeRight(lengthDifference2)
      x1 ++ x2
    }

    else
      s1.zip(s2).flatMap(_.productIterator)
  }
}