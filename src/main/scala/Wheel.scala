import java.lang.Math.floorMod

import Wheel.{keyboard, mod}

trait Wheel:
  def encode(char: Char): Char =
    val index = keyboard.indexOf(char)
    if index == -1 then throw Exception(f"Character '$char' not on keyboard")
    val newIndex = mod(mapping(index))
    keyboard.charAt(newIndex)
  
  def mapping(index: Int): Int
  
  def inverse: Wheel

object Wheel:
  
  def shift(num: Int, scale: Int = 1): Wheel = new Wheel:
    def mapping(index: Int) = index + num * scale
    def inverse = shift(num, -scale)
  
  def shiftPrevious(scale: Int = 1): Wheel = new Wheel:
    private var position: Int = 0
    
    def mapping(index: Int) =
      val newIndex = index + position * scale
      position = index
      newIndex
    
    def inverse = shiftPreviousInverse(-scale)

  def shiftPreviousInverse(scale: Int = 1): Wheel = new Wheel:
    private var position: Int = 0

    def mapping(index: Int) =
      val newIndex = index + position * scale
      position = mod(newIndex)
      newIndex

    def inverse = shiftPrevious(-scale)

  private def mod(index: Int) = floorMod(index, keyboard.size)
  
  private val keyboard = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.,?!'\" "
