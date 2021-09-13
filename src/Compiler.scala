import microKanren.*
import scalaKanren.*

import java.io.File
import java.io.PrintWriter

import scala.io.Source

object Compiler extends App {
    val input = Source.fromFile("src/Input.scm").mkString.strip()
    var i = 0
    
    val output = new File("src/Output.scala")
    output.delete()
    val writer = new PrintWriter(output)
    
    writer.write(Generator.imports)
    
    while (i < input.length) {
        val cursor = Lexer.lex(input, i)
        i = cursor._1
        val expression = Parser.parse(cursor._2)
        writer.write(Generator.generate(expression))
    }
    
    writer.close()
}
