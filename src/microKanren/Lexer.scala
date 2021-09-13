package microKanren

type Cursor = (Int, TokenNode)

implicit class CursorHelper(cursor: Cursor) {
    def quoted: Cursor = (cursor._1, TokenQuote(cursor._2))
    def unquoted: Cursor = (cursor._1, TokenUnquote(cursor._2))
    def quasiQuoted: Cursor = (cursor._1, TokenQuasiQuote(cursor._2))
}

object Lexer {
    val PL = '(' // parenthesis left
    val PR = ')' // parenthesis right
    val D = '.'  // dot
    val Q = '\'' // quote
    val UQ = ',' // un-quote
    val QQ = '`' // quasi-quote
    
    def isSymbol(char: Char): Boolean =
        Seq(PL, PR, D, Q, UQ, QQ).contains(char) || char.isWhitespace
    
    val tokenMap = Map(
        "call/fresh" -> TokenCF(),
        "conj" -> TokenConj(),
        "define" -> TokenDefine(),
        "disj" -> TokenDisj(),
        "==" -> TokenEq(),
        "lambda" -> TokenLambda()
    )
    
    def lex(input: String, idx: Int = 0): Cursor = {
        input(idx) match {
            case char if char.isWhitespace => lex(input, idx + 1)
            case D => (idx + 1, TokenDot())
            case Q => lex(input, idx + 1).quoted
            case UQ => lex(input, idx + 1).unquoted
            case QQ => lex(input, idx + 1).quasiQuoted
            case PL => {
                var i = idx + 1
                var pr = findParenRight(input, idx)
                var nodes: Seq[TokenNode] = Seq.empty
                
                while (i < pr) {
                    val cursor = lex(input, i)
                    i = cursor._1
                    nodes = nodes :+ cursor._2
                }
                
                (i + 1, TokenTree(nodes: _*))
            }
            case _ => {
                val i = input.indexWhere(isSymbol, idx + 1)
                val string = input.substring(idx, i)
                (i, tokenMap.getOrElse(string, Token(string)))
            }
        }
    }
    
    def findParenRight(input: String, parenLeft: Int): Int = {
        var parens = 1
        var i = parenLeft + 1
        
        while (parens > 0 && i < input.length) {
            input(i) match {
                case PL => parens += 1
                case PR => parens -= 1
                case _ => ()
            }
            i += 1
        }
        
        assert(parens == 0)
        i - 1
    }
}
