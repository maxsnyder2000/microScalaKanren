import scalaKanren.*

object Playground extends App {
    appendo(
        variable,
        variable,
        Cons("a", Cons("b", Cons("c", Cons("d", Cons("e", Null())))))
    )(EMPTY_STATE).take(10).reified(COUNTER).map(println)
}
