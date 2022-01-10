package graph

import graph.Schema.Key.Self
import graph.Schema.Node.keyFnImpl
import graph.Schema.{Edge, Node}

import scala.deriving.Mirror
import java.lang.constant.Constable
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
  sealed trait GraphObject {

    inline def keyFn[T, K](inline key: Key[T, K]): T => Any
  }
  trait Node[K <: Key[?, ?]] extends GraphObject {
    override transparent inline def keyFn[T, K](
        inline key: Key[T, K]
    ): T => Any = ${
      Node.keyFnImpl[T, K]('{ key })
    }

  }
  object Node {

    def keyFnImpl[T: Type, K: Type](key: Expr[Key[T, K]])(using
        Quotes
    ): Expr[T => Any] = '{ _.toString }
  }
  trait Edge[From <: Node[?], K <: Key[?, ?], To <: Node[?]] extends GraphObject
  sealed trait Key[T, K] {
    type Out
  }
  object Key {
    sealed trait #>[T, K <: String & Constable] extends Key[T, K]
    object #> {
      type Aux[T, K <: String & Constable, O] = #>[T, K] { type Out = O }
    }
    sealed trait Self[T <: Singleton] extends Key[T, T] {
      override type Out = T
    }
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
