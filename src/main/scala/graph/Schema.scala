package graph

import scala.deriving.Mirror

import java.lang.constant.Constable

type #>[Node, Key <: String & Constable] =
  NodeType[Node, Key]

trait GraphType
object GraphType {}
trait NodeType[Node, Key <: String & Constable] extends GraphType
trait EdgeType[In <: NodeType[?, ?], Edge, Out <: NodeType[?, ?]]
    extends GraphType

case class Person(name: String, age: Int)
case object LivesAt
case object Knows
case class Address(number: Int, street: String)

trait Schema[N <: NodeType[?, ?], E <: EdgeType[? <: N, ?, ? <: N]] {
  def addNode[A <: NodeType[?, ?]]: Schema[N | A, E] = new Schema[N | A, E] {}
  def addEdge[In <: N, Edge, Out <: N]: Schema[N, E | EdgeType[In, Edge, Out]] =
    new Schema[N, E | EdgeType[In, Edge, Out]] {}
  def addSchema[N0 <: NodeType[?, ?], E0 <: EdgeType[? <: N0, ?, ? <: N0]](
      that: Schema[N0, E0]
  ): Schema[N | N0, E | E0] = new Schema[N | N0, E | E0] {}
}

object Schema extends App {
  val empty = new Schema[Nothing, Nothing] {}
  type PersonNode = Person #> "name"
  type AddressNode = Address #> "number"
  type LivesAtEdge = EdgeType[PersonNode, LivesAt.type, AddressNode]
  type KnowsEdge = EdgeType[PersonNode, Knows.type, PersonNode]
  val test = new Schema[
    PersonNode | AddressNode,
    LivesAtEdge | KnowsEdge
  ] {}

  val social =
    empty.addNode[Person #> "name"].addEdge[PersonNode, Knows.type, PersonNode]
  val delivery = empty
    .addNode[Person #> "name"]
    .addNode[AddressNode]
    .addEdge[PersonNode, LivesAt.type, AddressNode]
  val socialDelivery = social.addSchema(delivery)

}
