package graph

import graph.Schema.Key.Self

import graph.Schema.{Edge, Node}
import scala.compiletime.*
import scala.quoted.*
import scala.deriving.*
import scala.deriving.Mirror
import java.lang.constant.Constable
import scala.Tuple.Union
import scala.quoted.{Expr, Quotes, Type}
import java.lang.constant.Constable

case class Person(name: String, age: Int)
case object LivesAt
case object Knows
case class Address(number: Int, street: String)

trait Schema[N <: Node[?, ?], E <: Edge[? <: N, ?, ?, ? <: N]] {
  def addNode[N0 <: Node[?,?]]: Schema[N | N0, E] = new Schema[N | N0, E] {}
  def addEdge[
      E0 <: Edge[? <: N, ?, ?, ? <: N]
  ]: Schema[N, E | E0] =
    new Schema[N, E | E0] {}
  def addSchema[N0 <: Node[?,?], E0 <: Edge[? <: N0, ? ,?, ? <: N0]](
      that: Schema[N0, E0]
  ): Schema[N | N0, E | E0] = new Schema[N | N0, E | E0] {}
}

object Schema {
  import Key._
  sealed trait GraphObject[T, O] {
    type K <: String & Constable
    private[GraphObject] val key0: Key.Aux[T, K, O]
    
    // transparent inline def keyFn[T, K](inline key: Key[T, K]): T => Any
  }
  sealed trait Node[T,O] extends GraphObject[T, O] {
    
  }
  object Node {
    transparent inline def apply[K <: Key[?, ?]]: Any =
      ${
        applyFnImpl[K]
      }
    private def applyFnImpl[K <: Key[?, ?]: Type](using
        Quotes
    ): Expr[Any] = {
      import quotes.reflect._

      def loop[L: Type, T: Type, K: Type]: TypeRepr = {
        Type.of[L] match
          case '[lHead *: lTail] =>
            Type.of[T] match
              case '[tHead *: tTail] =>
                if (TypeRepr.of[lHead] == TypeRepr.of[K]) TypeRepr.of[tHead]
                else loop[lTail, tTail, K]
          case '[EmptyTuple] => ???
      }

      Type.of[K] match {
        case '[Key[t, k]] =>
          Expr.summon[Mirror.ProductOf[t]].get match {
            case '{
                  $m: Mirror.ProductOf[t] {
                    type MirroredElemLabels = labels;
                    type MirroredElemTypes = types
                  }
                } =>
              loop[labels, types, k].asType match {
                case '[r] =>
                  '{
                    new Node[t, r] {
                      val key0 = new Key[t,k]{type T_ = t; type K_ = k; type O_ = r }
                    }
                  }
              }
          }
      }
    }
  }
  sealed trait Edge[From <: Node[_, _], T, O, To <: Node[_,_]]
      extends GraphObject[T,O]
  //
  sealed trait Key[T, K] {
    type O_
    val keyFn: T => O_
    def apply(t: T): O_ = keyFn(t)
  }

  object Key {
    def apply[T, K, O](keyFn0: T => O): Aux[T, K, O] = new Key[T, K] {
      self =>
      override type T_ = T
      override type K_ = K
      override type O_ = O
      override val keyFn: T => O_ = keyFn0
    }

    type Aux[T, K, O] = Key[T, K] { type T_ = T; type K_ = K; type O_ = O }

    sealed trait #>[T, K <: String & Constable] extends Key[T, K]
    sealed trait Self[T <: Singleton] extends Key[T, T]
  }

}
