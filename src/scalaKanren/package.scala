import scala.util.Try

package object scalaKanren {
    case class Variable(index: Int)
    type Value = Any
    type Substitution = Map[Variable, Value]
    type State = (Substitution, Variable)
    
    trait Pair
    case class Null() extends Pair
    case class Cons(car: Value, cdr: Value) extends Pair
    
    trait Stream
    case class ZeroStream() extends Stream
    case class MatureStream(car: State, cdr: Stream) extends Stream
    case class ImmatureStream(procedure: () => Stream) extends Stream
    
    type Goal = State => Stream
    
    private def walk(u: Value, s: Substitution): Value = {
        Try(u.asInstanceOf[Variable])
            .toOption
            .flatMap(s.get)
            .map(walk(_, s))
            .getOrElse(u)
    }
    
    private def ext_s(x: Variable, v: Value, s: Substitution): Substitution = {
        s + (x -> v)
    }
    
    def eq(u: Value, v: Value): Goal = {
        s_c => {
            val sOpt = unify(u, v, s_c._1)
            sOpt.map(s => MatureStream((s, s_c._2), MZERO)).getOrElse(MZERO)
        }
    }
    
    val MZERO: Stream = ZeroStream()
    
    private def unify(u: Value, v: Value, s: Substitution): Option[Substitution] = {
        val uWalk = walk(u, s)
        val vWalk = walk(v, s)
        
        (uWalk, vWalk) match {
            case (Variable(i1), Variable(i2)) if (i1 == i2) => Some(s)
            case (uVar @ Variable(_), _) => Some(ext_s(uVar, vWalk, s))
            case (_, vVar @ Variable(_)) => Some(ext_s(vVar, uWalk, s))
            case (Cons(uCar, uCdr), Cons(vCar, vCdr)) => {
                unify(uCar, vCar, s)
                    .flatMap { s2 =>
                        unify(uCdr, vCdr, s2)
                    }
            }
            case _ => if (uWalk == vWalk) Some(s) else None
        }
    }
    
    def call_fresh(f: Value => Goal): Goal = {
        s_c => {
            f(s_c._2)(s_c._1, Variable(s_c._2.index + 1))
        }
    }
    
    def conj(g1: Goal, g2: Goal): Goal = {
        s_c => {
            bind(g1(s_c), g2)
        }
    }
    
    def disj(g1: Goal, g2: Goal): Goal = {
        s_c => {
            mplus(g1(s_c), g2(s_c))
        }
    }
    
    private def bind(s: Stream, g: Goal): Stream = {
        s match {
            case MZERO => MZERO
            case MatureStream(car, cdr) => mplus(g(car), bind(cdr, g))
            case ImmatureStream(proc) => ImmatureStream(() => bind(proc(), g))
        }
    }
    
    private def mplus(s1: Stream, s2: Stream): Stream = {
        s1 match {
            case MZERO => s2
            case MatureStream(car, cdr) => MatureStream(car, mplus(cdr, s2))
            case ImmatureStream(proc) => ImmatureStream(() => mplus(s2, proc()))
        }
    }
    
    def variable: Variable = { COUNTER = COUNTER + 1; Variable(COUNTER) }
    var COUNTER = -1
    
    def EMPTY_STATE: State = (Map.empty, variable)
    
    implicit class StreamToList(stream: Stream) {
        def pull: Stream = {
            var immature = true
            var curr = stream
            
            while (immature) {
                curr match {
                    case ImmatureStream(proc) => curr = proc()
                    case _ => immature = false
                }
            }
            
            curr
        }
        
        def take(n: Int = -1): Stream = {
            if (n == 0) {
                MZERO
            } else {
                stream.pull match {
                    case MatureStream(car, cdr) => MatureStream(car, cdr.take(n - 1))
                    case _ => stream
                }
            }
        }
        
        def reify(i: Int = 0): Seq[Value] = {
            stream match {
                case MatureStream(car, cdr) => reify_state(car, i) +: cdr.reify(i)
                case _ => Seq()
            }
        }
        
        private def reify_state(s_c: State, i: Int): Value = {
            val v = walk_(Variable(i), s_c._1)
            walk_(v, reify_s(v, Map.empty))
        }
        
        private def reify_s(v: Value, s: Substitution): Substitution = {
            walk(v, s) match {
                case Cons(car, cdr) => reify_s(cdr, reify_s(car, s))
                case v: Variable => s + (v -> reify_name(s.size))
                case _ => s
            }
        }
        
        private def reify_name(n: Int): String = "_." + n
        
        private def walk_(v: Value, s: Substitution): Value = {
            walk(v, s) match {
                case Cons(car, cdr) => Cons(walk_(car, s), walk_(cdr, s))
                case v => v
            }
        }
        
        def reified(n: Int): Seq[Seq[Value]] = {
            if (n == 0) {
                Seq.empty
            } else {
                var rs = stream.reify(0).map(Seq(_))
                
                for (i <- 1 until n) {
                    val r = stream.reify(i)
                    rs = for (j <- 0 until rs.length) yield {
                        rs(j) :+ r(j)
                    }
                }
                
                rs
            }
        }
    }
}
