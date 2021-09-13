package microKanren

trait TokenNode

case class Token(string: String) extends TokenNode
case class TokenTree(nodes: TokenNode*) extends TokenNode

case class TokenDot() extends TokenNode
case class TokenQuote(node: TokenNode) extends TokenNode
case class TokenUnquote(node: TokenNode) extends TokenNode
case class TokenQuasiQuote(node: TokenNode) extends TokenNode

case class TokenCF() extends TokenNode
case class TokenConj() extends TokenNode
case class TokenDefine() extends TokenNode
case class TokenDisj() extends TokenNode
case class TokenEq() extends TokenNode
case class TokenLambda() extends TokenNode
