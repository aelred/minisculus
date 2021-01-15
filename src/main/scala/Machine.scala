import Wheel.{shift, shiftPrevious}

class Machine(wheels: Wheel*):
  def encode(string: String): String =
    string map { char =>
      wheels.foldRight(char) { _ encode _ }
    }

  def decode(string: String): String = decoder.encode(string)

  def decoder: Machine = Machine(wheels.map(_.inverse).reverse: _*)


object Machine:
  def markI(wheel: Int) = Machine(shift(wheel))
  def markII(wheel1: Int, wheel2: Int) = Machine(shift(wheel1), shift(wheel2, -2))
  def markIV(wheel1: Int, wheel2: Int) = Machine(shift(wheel1), shift(wheel2, -2), shiftPrevious(2))