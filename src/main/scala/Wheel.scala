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
  def shiftPrevious(scale: Int = 1): Wheel = new Wheel:
    private var position: Int = 0
    
    def mapping(index: Int) =
      val newIndex = index + position * scale
      position = index
      newIndex
    
    def inverse = shiftPreviousInverse(-scale)

  private def shiftPreviousInverse(scale: Int = 1): Wheel = new Wheel:
    private var position: Int = 0

    def mapping(index: Int) =
      val newIndex = index + position * scale
      position = mod(newIndex)
      newIndex

    def inverse = shiftPrevious(-scale)

  private def mod(index: Int) = floorMod(index, keyboard.size)
  
  private val keyboard = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.,?!'\" "
