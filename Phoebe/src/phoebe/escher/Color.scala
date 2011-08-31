package phoebe.escher

import java.awt.{Color => AwtColor}
import phoebe.Prelude._

class Color private[phoebe](private[phoebe] val awtColor: AwtColor) extends Proxy {

  override def self = awtColor
  override def toString = "Color(red = %d, green = %d, blue = %d)" format (red, green, blue)

  def red: Int = awtColor.getRed
  def green: Int = awtColor.getGreen
  def blue: Int = awtColor.getBlue
  def alpha: Int = awtColor.getAlpha
  def rgb: Int = awtColor.getRGB
  def transparency: Int = awtColor.getTransparency

  def rgbComponents: (Float, Float, Float) = {
    val c = awtColor.getRGBColorComponents(null)
    (c(0), c(1), c(2))
  }

  def rgbaComponents: (Float, Float, Float, Float) = {
    val c = awtColor.getRGBComponents(null)
    (c(0), c(1), c(2), c(3))
  }

  def brighter: Color = new Color(awtColor.brighter)
  def darker: Color = new Color(awtColor.darker)
}

object Color {
  def apply(r: Int, g: Int, b: Int, a: Int = 255): Color = new Color(new AwtColor(r, g, b, a))
  def apply(rgb: Int): Color = new Color(new AwtColor(rgb))
  def fromHsb(hue: Float, saturation: Float, brightness: Float): Color =
    new Color(AwtColor.getHSBColor(hue, saturation, brightness))

  def hsbToRgb(hue: Float, saturation: Float, brightness: Float): Int =
    AwtColor.HSBtoRGB(hue, saturation, brightness)

  def rgbToHsb(r: Int, g: Int, b: Int): (Float, Float, Float) = {
    val hsb = AwtColor.RGBtoHSB(r, g, b, null)
    (hsb(0), hsb(1), hsb(2))
  }

  def unapply(rgb: Int): Option[(Int, Int, Int)] = {
    val toKeep = (1 << 8) - 1
    val r = (rgb >>> 16) & toKeep
    val g = (rgb >>> 8) & toKeep
    val b = rgb & toKeep
    Some((r, g, b))
  }

  //
  // The following are the X11 web colors as specified in the CSS3 specification
  //

  // Red colors
  val IndianRed = Color(0xcd5c5c)
  val LightCoral = Color(0xf08080)
  val Salmon = Color(0xfa8072)
  val DarkSalmon = Color(0xe9967a)
  val LightSalmon = Color(0xffa07a)
  val Crimson = Color(0xdc143c)
  val Red = Color(0xff0000)
  val FireBrick = Color(0xb22222)
  val DarkRed = Color(0x8b0000)

  // Pink colors
  val Pink = Color(0xffc0cb)
  val LightPink = Color(0xffb6c1)
  val HotPink = Color(0xff69b4)
  val DeepPink = Color(0xff1493)
  val MediumVioletRed = Color(0xc71585)
  val PaleVioletRed = Color(0xdb7093)

  // Orange colors
  val Coral = Color(0xff7f50)
  val Tomato = Color(0xff6347)
  val OrangeRed = Color(0xff4500)
  val DarkOrange = Color(0xff8c00)
  val Orange = Color(0xffa500)

  // Yellow colors
  val Gold = Color(0xffd700)
  val Yellow = Color(0xffff00)
  val LightYellow = Color(0xffffe0)
  val LemonChiffon = Color(0xfffacd)
  val LightGoldenrodYellow = Color(0xfafad2)
  val PapayaWhip = Color(0xffefd5)
  val Moccasin = Color(0xffe4b5)
  val PeachPuff = Color(0xffdab9)
  val PaleGoldenrod = Color(0xeee8aa)
  val Khaki = Color(0xf0e68c)
  val DarkKhaki = Color(0xbdb76b)

  // Purple colors
  val Lavender = Color(0xe6e6fa)
  val Thistle = Color(0xd8bfd8)
  val Plum = Color(0xdda0dd)
  val Violet = Color(0xee82ee)
  val Orchid = Color(0xda70d6)
  val Fuchsia = Color(0xff00ff)
  val Magenta = Color(0xff00ff)
  val MediumOrchid = Color(0xba55d3)
  val MediumPurple = Color(0x9370db)
  val Amethyst = Color(0x9966cc)
  val BlueViolet = Color(0x8a2be2)
  val DarkViolet = Color(0x9400d3)
  val DarkOrchid = Color(0x9932cc)
  val DarkMagenta = Color(0x8b008b)
  val Purple = Color(0x800080)
  val Indigo = Color(0x4b0082)
  val SlateBlue = Color(0x6a5acd)
  val DarkSlateBlue = Color(0x483d8b)
  val MediumSlateBlue = Color(0x7b68ee)

  // Green colors
  val GreenYellow = Color(0xadff2f)
  val Chartreuse = Color(0x7fff00)
  val LawnGreen = Color(0x7cfc00)
  val Lime = Color(0x00ff00)
  val LimeGreen = Color(0x32cd32)
  val PaleGreen = Color(0x98fb98)
  val LightGreen = Color(0x90ee90)
  val MediumSpringGreen = Color(0x00fa9a)
  val SpringGreen = Color(0x00ff7f)
  val MediumSeaGreen = Color(0x3cb371)
  val SeaGreen = Color(0x2e8b57)
  val ForestGreen = Color(0x228b22)
  val Green = Color(0x008000)
  val DarkGreen = Color(0x006400)
  val YellowGreen = Color(0x9acd32)
  val OliveDrab = Color(0x6b8e23)
  val Olive = Color(0x808000)
  val DarkOliveGreen = Color(0x556b2f)
  val MediumAquamarine = Color(0x66cdaa)
  val DarkSeaGreen = Color(0x8fbc8f)
  val LightSeaGreen = Color(0x20b2aa)
  val DarkCyan = Color(0x008b8b)
  val Teal = Color(0x008080)

  // Blue/Cyan colors
  val Aqua = Color(0x00ffff)
  val Cyan = Color(0x00ffff)
  val LightCyan = Color(0xe0ffff)
  val PaleTurquoise = Color(0xafeeee)
  val Aquamarine = Color(0x7fffd4)
  val Turquoise = Color(0x40e0d0)
  val MediumTurquoise = Color(0x48d1cc)
  val DarkTurquoise = Color(0x00ced1)
  val CadetBlue = Color(0x5f9ea0)
  val SteelBlue = Color(0x4682b4)
  val LightSteelBlue = Color(0xb0c4de)
  val PowderBlue = Color(0xb0e0e6)
  val LightBlue = Color(0xadd8e6)
  val SkyBlue = Color(0x87ceeb)
  val LightSkyBlue = Color(0x87cefa)
  val DeepSkyBlue = Color(0x00bfff)
  val DodgerBlue = Color(0x1e90ff)
  val CornflowerBlue = Color(0x6495ed)
  val RoyalBlue = Color(0x4169e1)
  val Blue = Color(0x0000ff)
  val MediumBlue = Color(0x0000cd)
  val DarkBlue = Color(0x00008b)
  val Navy = Color(0x000080)
  val MidnightBlue = Color(0x191970)

  // Brown colors
  val Cornsilk = Color(0xfff8dc)
  val BlanchedAlmond = Color(0xffebcd)
  val Bisque = Color(0xffe4c4)
  val NavajoWhite = Color(0xffdead)
  val Wheat = Color(0xf5deb3)
  val BurlyWood = Color(0xdeb887)
  val Tan = Color(0xd2b48c)
  val RosyBrown = Color(0xbc8f8f)
  val SandyBrown = Color(0xf4a460)
  val Goldenrod = Color(0xdaa520)
  val DarkGoldenrod = Color(0xb8860b)
  val Peru = Color(0xcd853f)
  val Chocolate = Color(0xd2691e)
  val SaddleBrown = Color(0x8b4513)
  val Sienna = Color(0xa0522d)
  val Brown = Color(0xa52a2a)
  val Maroon = Color(0x800000)

  // White colors
  val White = Color(0xffffff)
  val Snow = Color(0xfffafa)
  val Honeydew = Color(0xf0fff0)
  val MintCream = Color(0xf5fffa)
  val Azure = Color(0xf0ffff)
  val AliceBlue = Color(0xf0f8ff)
  val GhostWhite = Color(0xf8f8ff)
  val WhiteSmoke = Color(0xf5f5f5)
  val Seashell = Color(0xfff5ee)
  val Beige = Color(0xf5f5dc)
  val OldLace = Color(0xfdf5e6)
  val FloralWhite = Color(0xfffaf0)
  val Ivory = Color(0xfffff0)
  val AntiqueWhite = Color(0xfaebd7)
  val Linen = Color(0xfaf0e6)
  val LavenderBlush = Color(0xfff0f5)
  val MistyRose = Color(0xffe4e1)

  // Gray colors
  val Gainsboro = Color(0xdcdcdc)
  val LightGrey = Color(0xd3d3d3)
  val Silver = Color(0xc0c0c0)
  val DarkGray = Color(0xa9a9a9)
  val Gray = Color(0x808080)
  val DimGray = Color(0x696969)
  val LightSlateGray = Color(0x778899)
  val SlateGray = Color(0x708090)
  val DarkSlateGray = Color(0x2f4f4f)
  val Black = Color(0x000000)
}