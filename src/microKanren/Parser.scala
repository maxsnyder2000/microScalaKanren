package microKanren

import scala.util.Try

object Parser {
    def parse(node: TokenNode)(implicit quoted: Boolean = false): Expression = {
        node match {
            case Token(token) if token.toIntOption.isDefined =>
                ExpressionInt(token.toInt)
            case Token(token) if quoted =>
                ExpressionString(token)
            case Token(token) =>
                ExpressionVariable(token)
            case TokenQuote(node) =>
                parse(node)(true)
            case TokenUnquote(Token(token)) =>
                ExpressionVariable(token)
            case TokenQuasiQuote(node) =>
                parse(node)(true)
            case TokenTree(TokenCF(), node) =>
                ExpressionCF(parse(node))
            case TokenTree(TokenConj(), node1, node2) =>
                ExpressionConj(parse(node1), parse(node2))
            case TokenTree(TokenDefine(), Token(name), node) =>
                ExpressionDefine(name, parse(node))
            case TokenTree(TokenDisj(), node1, node2) =>
                ExpressionDisj(parse(node1), parse(node2))
            case TokenTree(TokenEq(), node1, node2) =>
                ExpressionEq(parse(node1), parse(node2))
            case TokenTree(TokenLambda(), TokenTree(nodes: _*), node) =>
                val tokens = Try(nodes.asInstanceOf[Seq[Token]])
                assert(tokens.isSuccess)
                ExpressionLambda(tokens.get.map(_.string), parse(node))
            case TokenTree() if quoted =>
                ExpressionNull()
            case TokenTree(node1, TokenDot(), node2) if quoted =>
                ExpressionCons(parse(node1), parse(node2))
            case TokenTree(nodes: _*) if quoted =>
                ExpressionCons(parse(nodes(0)), parse(TokenTree(nodes.drop(1): _*)))
            case TokenTree(nodes: _*) =>
                ExpressionApply(parse(nodes(0)), nodes.drop(1).map(parse): _*)
        }
    }
}
