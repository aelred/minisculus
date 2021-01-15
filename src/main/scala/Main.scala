import Machine.{markI, markII, markIV}

import scala.annotation.Annotation

@main def questions =
  println(markI(6).encode("Strong NE Winds!"))
    
  println(markII(9, 3).encode("The Desert Fox will move 30 tanks to Calais at dawn"))
    
  println(markIV(4, 7).encode("The white cliffs of Alghero are visible at night"))
    
  println(markIV(7, 2)
    .decode("WZyDsL3u'0TfxP06RtSSF 'DbzhdyFIAu2 zF f5KE\"SOQTNA8A\"NCKPOKG5D9GSQE'M86IGFMKE6'K4pEVPK!bv83I"))

  val message = "QT4e8MJYVhkls.27BL9,.MSqYSi'IUpAJKWg9Ul9p4o8oUoGy'ITd4d0AJVsLQp4kKJB2rz4dxfahwUa\"Wa.MS!k4hs2yY3k8ymnla.MOTxJ6wBM7sC0srXmyAAMl9t\"Wk4hs2yYTtH0vwUZp4a\"WhB2u,o6.!8Zt\"Wf,,eh5tk8WXv9UoM99w2Vr4!.xqA,5MSpWl9p4kJ2oUg'6evkEiQhC'd5d4k0qA'24nEqhtAQmy37il9p4o8vdoVr!xWSkEDn?,iZpw24kF\"fhGJZMI8nkI"
    
  for wheel1 <- 0 to 10 do
    for wheel2 <- 0 to 10 do 
      val decoded: String = markIV(wheel1, wheel2).decode(message)
      if decoded.contains("BUNKER") && decoded.contains("FURLIN") then
        println(f"$wheel1, $wheel2:")
        println(decoded)