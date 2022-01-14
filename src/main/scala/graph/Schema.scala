package graph

import graph.Schema.Key.Self
import graph.Schema.Node.keyFnImpl
import graph.Schema.{Edge, Node}
import scala.compiletime.*
import scala.quoted.*
import scala.deriving.*
import scala.deriving.Mirror
import java.lang.constant.Constable
import scala.Tuple.Union
import scala.quoted.{Expr, Quotes, Type}

case class Person(name: String, age: Int)
case object LivesAt
case object Knows
case class Address(number: Int, street: String)

trait Schema[N <: Node[?], E <: Edge[? <: N, ?, ? <: N]] {
  def addNode[N0 <: Node[?]]: Schema[N | N0, E] = new Schema[N | N0, E] {}
  def addEdge[
      E0 <: Edge[? <: N, ?, ? <: N]
  ]: Schema[N, E | E0] =
    new Schema[N, E | E0] {}
  def addSchema[N0 <: Node[?], E0 <: Edge[? <: N0, ?, ? <: N0]](
      that: Schema[N0, E0]
  ): Schema[N | N0, E | E0] = new Schema[N | N0, E | E0] {}
}

object Schema {
  import Key._
  sealed trait GraphObject[K <: Key.Aux[?, ?, ?]] {
    val key: K

    // transparent inline def keyFn[T, K](inline key: Key[T, K]): T => Any
  }
  sealed trait Node[K <: Key.Aux[?, ?, ?]] extends GraphObject[K] {}
  object Node {
    transparent inline def apply[K <: Key[?, ?]]: Node[Key.Aux[Any, Any, Any]] =
      ${
        applyFnImpl[K]
      }
    private def applyFnImpl[K <: Key[?, ?]: Type](using
        Quotes
    ): Expr[Node[Key.Aux[Any, Any, Any]]] = {
      import quotes.reflect._
      def loop[L: Type, T: Type, K: Type]: TypeRepr =
        Type.of[L] match
          case '[lHead *: lTail] =>
            Type.of[T] match
              case '[tHead *: tTail] => {
                if (Type.of[lHead] == Type.of[K]) TypeRepr.of[tHead]
                else loop[lTail, tTail, T]
              }

      Type.of[K] match {
        case '[Key[t, k]] => {
          Expr.summon[Mirror.ProductOf[k]].get match {
            case '{
                  $m: Mirror.ProductOf[k] {
                    type MirroredElemLabels = labels;
                    type MirroredElemTypes = types
                  }
                } =>
              loop[labels, types, k]

            case _ =>
              throw new IllegalStateException(
                "labels and types must be the same length"
              )

          }
        }
        case _ => throw new Exception("uh oh")
      }
    }
  }
  trait Edge[From <: Node[?], K <: Key.Aux[?, ?, ?], To <: Node[?]]
      extends GraphObject[K]

  sealed trait Key[T, K] {
    type Out
    val keyFn: T => Out
  }

  object Key {
    type Aux[T, K <: String & Constable, O] = Key[T, K] { type Out = O }

    sealed trait #>[T, K <: String & Constable] extends Key[T, K]
    sealed trait Self[T <: Singleton] extends Key[T, T]
  }

}

object Test extends App {
  import Schema.Key._
  val empty = new Schema[Nothing, Nothing] {}
  type PersonNode = Node[Person #> "name"]
  type AddressNode = Node[Address #> "number"]
  type LivesAtEdge = Edge[PersonNode, Self["LivesAt"], AddressNode]
  type KnowsEdge = Edge[PersonNode, Self[Knows.type], PersonNode]
  val test = new Schema[
    PersonNode | AddressNode,
    LivesAtEdge | KnowsEdge
  ] {}

  val social =
    empty.addNode[PersonNode].addEdge[KnowsEdge]
  val delivery = empty
    .addNode[PersonNode]
    .addNode[AddressNode]
    .addEdge[LivesAtEdge]
  val socialDelivery = social.addSchema(delivery)
}
