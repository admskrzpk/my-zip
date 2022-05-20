import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatestplus.scalacheck.Checkers
import org.scalatestplus.scalacheck.Checkers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class myZipSpec extends AnyFlatSpec with should.Matchers with Checkers {

  import MyZip._

  //check((a: Seq[Int], b: Seq[String]) => myZip(a, b).size == (a ++ b).size)

  "myzip" should "return empty list if both input lists are empty." in {
    forAll { (a: Seq[Int], b: Seq[String]) =>
      whenever (a.nonEmpty && b.nonEmpty)  {
        myZip(a, b).take(2) shouldBe Seq(a.head, b.head)
      }
    }
  }

  "myzip" should "return empty Seq if both input Seq are empty" in {
    val s1 = Seq()
    val s2 = Seq()
    val expected = Seq()

    myZip(s1, s2) shouldBe expected
  }


  "myzip" should "intertwine two sequences" in {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq('a', 'a', 'a')
    val expected = Seq(1, 'a', 2, 'a', 3, 'a')

    myZip(s1, s2) shouldBe expected
  }

  "myzip" should "intertwine two sequences of different size, first is shorter" in {
    val s1 = Seq(1, 2)
    val s2 = Seq('a', 'a', 'a')
    val expected = Seq(1, 'a', 2, 'a', 'a')

    myZip(s1, s2) shouldBe expected
  }
  "myzip" should "intertwine two sequences of different size" in {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq('a', 'a')
    val expected = Seq(1, 'a', 2, 'a', 3)

    myZip(s1, s2) shouldBe expected
  }
}