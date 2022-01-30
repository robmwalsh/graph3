package graph

import graph.Schema.{Edge, Node}

object Test extends App {
  import Schema.Key.*
  val empty = new Schema[Nothing, Nothing] {}
  val PersonNode = Node[Person #> "name"]
  val AddressNode = Node[Address #> "number"]
  
  ///val LivesAtEdge = Edge[PersonNode, Self["LivesAt"], AddressNode]
  /*type KnowsEdge = Edge[PersonNode, Self[Knows.type], PersonNode]
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
  val socialDelivery = social.addSchema(delivery)*/
}
