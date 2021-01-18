import Wheel.{shift, shiftPrevious}

/** Encoding machine, comprises a sequence of [[Wheel]]s */
class Machine(wheels: Wheel*):
  /** Encode a string of characters */
  def encode(string: String): String =
    string map { char =>
      wheels.foldRight(char) { _ encode _ }
    }

  /** Decode a string of characters such that `machine.decode(machine.encode(s)) == s` */
  def decode(string: String): String = decoder.encode(string)

  /** A machine that can decode messages encoded by the original machine */
  def decoder: Machine = Machine(wheels.map(_.inverse).reverse: _*)


object Machine:
  def markI(wheel: Int) = Machine(shift(wheel))
  def markII(wheel1: Int, wheel2: Int) = Machine(shift(wheel1), shift(wheel2 * -2))
  def markIV(wheel1: Int, wheel2: Int) = Machine(shift(wheel1), shift(wheel2 * -2), shiftPrevious(2))