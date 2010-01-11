package com.novocode.squery.combinator

import com.novocode.squery.combinator.sql.InsertBuilder
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}

class CombinatorInsertInvoker[T] (column: ColumnBase[T]) {

  lazy val insertStatement = new InsertBuilder(column).buildInsert

  def insertAutoInc(value: T)(implicit session: Session) = {
    val st = session.allocPSAutoInc(insertStatement)
    try {
      st.clearParameters
      column.setParameter(new PositionedParameters(st), Some(value))
      val ret = st.executeUpdate
      val rs = st.getGeneratedKeys()
      val id = if (rs.next) rs.getInt(1) else -1
      (ret, id)
    } finally session.freePS(insertStatement, st)
  }

  def insert(value: T)(implicit session: Session) = {
    val st = session.allocPS(insertStatement)
    try {
      st.clearParameters
      column.setParameter(new PositionedParameters(st), Some(value))
      st.executeUpdate
    } finally session.freePS(insertStatement, st)
  }

  
  def insertAll(values: T*)(implicit session: Session): Int = (0 /: values) { _ + insert(_) }
}
