package com.novocode.squery.combinator

trait QueryModifier extends Node

sealed abstract class Ordering(val by: Node) extends QueryModifier { def nodeChildren = by :: Nil }

object Ordering {
  final case class Asc(override val by: Node) extends Ordering(by) {
    override def toString = "Ordering.Asc"
  }

  final case class Desc(override val by: Node) extends Ordering(by) {
    override def toString = "Ordering.Desc"
  }
}

final case class Matching(by: List[NamedColumn[_]], what: Column[_], modifier: Option[SearchModifier.Value]) extends QueryModifier {
  def nodeChildren = by
  override def toString = "Matching"
}

final case class Grouping(val by: Node) extends QueryModifier {
  def nodeChildren = by :: Nil
  override def toString = "Grouping"
}
