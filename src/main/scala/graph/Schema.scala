package graph

import java.lang.constant.Constable

trait GraphType
object GraphType
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
}

object Schema extends App {
  val empty = new Schema[Nothing, Nothing] {}

  val test = new Schema[
    NodeType[Person, "name"] | NodeType[Address, "number"],
    EdgeType[
      NodeType[Person, "name"],
      LivesAt.type,
      NodeType[Address, "number"]
    ]
  ] {}

  val a = empty.addNode[NodeType[Person, "name"]]
  val b = a.addNode[NodeType[Address, "number"]]

  val c = b.addEdge[NodeType[Person, "name"], LivesAt.type, NodeType[
    Address,
    "number"
  ]]
}
