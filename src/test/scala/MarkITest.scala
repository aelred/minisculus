import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import Machine.markI

import scala.language.implicitConversions

class MarkITest extends AnyFreeSpec with Matchers {
  "MarkI(0).encode is identity" in {
    markI(0).encode("Hello world") shouldBe "Hello world"
  }

  "MarkI(n).encode should shift all chars by n" in {
    markI(2).encode("abc") shouldBe "cde"
  }

  "Wheel should wrap around" in {
    markI(1).encode(" ") shouldBe "0"
    markI(-1).encode("0") shouldBe " "
  }

  "MarkI(-n).encode is inverse of MarkI(n).encode" in {
    markI(-5).encode(markI(5).encode("Hello world")) shouldBe "Hello world"
  }
}
