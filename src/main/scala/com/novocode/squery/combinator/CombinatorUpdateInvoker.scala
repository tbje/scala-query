package com.novocode.squery.combinator

import com.novocode.squery.combinator.sql.QueryBuilder
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}

class CombinatorUpdateInvoker[T] (query: Query[Projection[T]]) {

  lazy val updateStatement = QueryBuilder.buildUpdate(query, NamingContext())

  def update(value: T)(implicit session: Session): Int = {
    val st = session.allocPS(updateStatement._1)
    try {
      st.clearParameters
      query.value.setParameter(new PositionedParameters(st), Some(value))
      st.executeUpdate
    } finally session.freePS(updateStatement._1, st)
  }
}

class ColumnBaseUpdateInvoker[T] (query: Query[ColumnBase[T]]) {

  lazy val updateStatement = QueryBuilder.buildUpdate(query, NamingContext())

  def update(value: T)(implicit session: Session): Int = {
    val st = session.allocPS(updateStatement._1)
    try {
      st.clearParameters
      query.value.setParameter(new PositionedParameters(st), Some(value))
      st.executeUpdate
    } finally session.freePS(updateStatement._1, st)
  }
}
