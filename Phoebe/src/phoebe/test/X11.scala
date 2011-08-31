package phoebe.test

import io.Source
import java.io.PrintWriter

//
// The code I used to generate all the X11 web colors in the Color object (used the table from
// X11's Wikipedia entry as my input)
//

object X11 {
  def main(args: Array[String]) {
    val code = Source.fromFile("X11.txt").getLines.map { line =>
      if(line startsWith "//")
        line
      else {
        val Array(name, h1, h2, h3, _, _, _) = line.split("[ \t\r\n]+")
        val hex = "0x" + (h1 + h2 + h3).toLowerCase
        "val %s = Color(%s)" format (name, hex)
      }
    }
    val out = new PrintWriter("HakunaMatata.txt")
    code foreach out.println
    out.flush()
    out.close()
  }
}