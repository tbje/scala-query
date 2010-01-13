package test

import junit.framework.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.Implicit._
import com.novocode.squery.session._
import com.novocode.squery.session.SessionFactory._


@RunWith(classOf[JUnit4])
class UpdateTest {

  @Test
  def should_test_update_on_projection {
	  object Users extends Table[(Int, String, String)]("users") {
	      def id = column[Int]("id")
	      def first = column[String]("first")
	      def last = column[String]("last")
	      def * = id ~ first ~ last
	  }
 	  val sp = new DriverManagerSessionFactory("jdbc:h2:mem:test1", "org.h2.Driver")

 	  sp withSession {
 		  Users.createTable
 		  val id = 1
 		  Users.insert(id, "first", "last")
 		  val query = for(u <- Users where {_.id is id.bind }) yield u
 		  val updateQuery = for (u <- Users where {_.id is id}) yield u.*.tail
 		  updateQuery.update("new first", "new last")
 		  assertEquals("new first",  query.first._2)
 	  }
  }
  
}
