import java.lang.Math.floorMod

import Wheel.{keyboard, mod}

/** Wheel in an encoding machine */
trait Wheel:

  /** Encode a [[Char]] to another [[Char]] */
  def encode(char: Char): Char =
    val index = keyboard.indexOf(char)
    if index == -1 then throw Exception(f"Character '$char' not on keyboard")
    val newIndex = mod(mapping(index))
    keyboard.charAt(newIndex)


  /** Defines how characters are mapped (as integers) */
  protected def mapping(index: Int): Int


  /** Invert the wheel, such that `wheel.encode(wheel.inverse.encode(c)) == c` */
  def inverse: Wheel


object Wheel:

  /** Simple shift wheel that shifts a charater a certain number of steps */
  def shift(num: Int): Wheel = new Wheel:
    def mapping(index: Int) = index + num
    def inverse = shift(-num)


  /** Shift wheel based on index of previous character */
  def shiftPrevious(scale: Int = 1, initialPosition: Int = 0): Wheel = new MutWheel(scale, initialPosition):
    def rotate(index: Int, newIndex: Int) = index
    def inverse = shiftPreviousInverse(-scale, initialPosition)

  
  private def shiftPreviousInverse(scale: Int, initialPosition: Int): Wheel = new MutWheel(scale, initialPosition):
    def rotate(index: Int, newIndex: Int) = newIndex
    def inverse = shiftPrevious(-scale, initialPosition)

  
  /** A wheel that rotates after each character */
  private abstract class MutWheel(scale: Int, initialPosition: Int) extends Wheel:
    private var position: Int = initialPosition
    
    def mapping(index: Int): Int =
      val newIndex = index + position * scale
      position = mod(rotate(index, newIndex))
      newIndex
  
    def rotate(index: Int, newIndex: Int): Int

  
  private def mod(index: Int) = floorMod(index, keyboard.size)

  private val keyboard = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.,?!'\" "
