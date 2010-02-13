package com.novocode.squery.combinator

import java.io.PrintWriter


class Query[+E](val value: E, val cond: List[ColumnBase[_]],
                val groupBy: List[GroupBy.Grouping], val orderBy: List[Ordering], val limit: Option[(Int, Int)]) extends Node {

  def select[F](f: E => Query[F]): Query[F] = {
    val q = f(value)
    limit match { 
      case Some(x) => Some(x)
      case None => q.limit
    }
    new Query(q.value, cond ::: q.cond, groupBy ::: q.groupBy, orderBy ::: q.orderBy, limit)
  }

  def where[T](f: E => ColumnBase[T])(implicit wt: Query.WhereType[T]): Query[E] = {
        new Query(value, f(value) :: cond, groupBy, orderBy, limit)
  }
  
  // Methods for use in for-comprehensions
  def flatMap[F](f: E => Query[F]): Query[F] = select(f)
  def map[F](f: E => F): Query[F] = flatMap(v => Query(f(v)))
  //def filter(f: E => BooleanColumn): Query[E] = where(f)

  def >>[F](q: Query[F]): Query[F] = flatMap(_ => q)

  def nodeChildren = Node(value) :: cond.map(Node.apply) ::: groupBy ::: orderBy
  override def nodeNamedChildren = (Node(value), "select") :: cond.map(n => (Node(n), "where")) :::
    groupBy.map(o => (o, "groupBy")) ::: orderBy.map(o => (o, "orderBy"))

  override def toString = "Query"

  def orderBy(by : Ordering*): Query[E] = new Query(value, cond, groupBy, orderBy ::: by.toList, limit)
  
  def limit(offset: Int, number: Int): Query[E] = new Query(value, cond, groupBy, orderBy, limit match { 
      case Some(x) => Some(x)
      case None => Some((offset, number))
    })
}
object Query extends Query[Unit]((), Nil, Nil, Nil, None) {
  def apply[E](value: E) = new Query(value, Nil, Nil, Nil, None)
 
  trait WhereType[T]
}

class QueryOfColumnOps[E <: ColumnBase[_]](q: Query[E]) {
  
  def union(other: Query[E]): Query[E] = {
    val u = Union(false, q, other)
    Query(q.value.mapOp(n => Union.UnionPart(n, u)))
  }
}
