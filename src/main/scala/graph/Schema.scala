package graph

import scala.deriving.Mirror

import java.lang.constant.Constable
import Key._
import GraphType._

sealed trait Key[T, K] {
  type Key
}
object Key {
  sealed trait #>[T, K <: String & Constable] extends Key[T, K]
  // key is tuple of node keys
  sealed trait Self[T <: Singleton] extends Key[T, T]
}

sealed trait GraphType {

}
object GraphType {
  trait Node[K <: #>[?, ?]] extends GraphType {
  trait Edge[From <: Node[?], K <: Key[?, ?], To <: Node[?]] extends GraphType
}

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

object Schema extends App {
  val empty = new Schema[Nothing, Nothing] {}
  type PersonNode = Node[Person #> "name"]
  type AddressNode = Node[Address #> "number"]
  type LivesAtEdge = Edge[PersonNode, Label["LivesAt"], AddressNode]
  type KnowsEdge = Edge[PersonNode, Label[Knows.type], PersonNode]
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
