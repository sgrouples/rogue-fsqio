// Copyright 2011 Foursquare Labs Inc. All Rights Reserved.

package io.fsq

package object rogue {

  type InitialState = Unordered
    with Unselected
    with Unlimited
    with Unskipped
    with HasNoOrClause
    with Unhinted
    with ShardKeyNotSpecified
  type OrderedState = Ordered
    with Unselected
    with Unlimited
    with Unskipped
    with HasNoOrClause
    with ShardKeyNotSpecified

  type SimpleQuery[M] = Query[M, M, InitialState]
  type OrderedQuery[M] = Query[M, M, OrderedState]

  trait Sharded

  trait ShardKey[V] {
    def name: String
    def eqs(v: V) = new EqClause(this.name, v) with ShardKeyClause
    def in[L](vs: L)(implicit ev: L => Iterable[V]) =
      new InQueryClause(this.name, QueryHelpers.validatedList(vs.toSet))
        with ShardKeyClause
  }

}
