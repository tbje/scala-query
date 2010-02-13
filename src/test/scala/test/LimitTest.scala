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
class LimitTest {

  @Test
  def should {
	  object Users extends Table[(Int, String, String)]("users") {
	      def id = column[Int]("id")
	      def first = column[String]("first")
	      def last = column[String]("last")
	      def * = id ~ first ~ last
	  }
 	  val sp = new DriverManagerSessionFactory("jdbc:h2:mem:test1", "org.h2.Driver")

 	  sp withSession {
 		  Users.createTable
 		  Users.insert(1, "first1", "last1")
 		  Users.insert(2, "first2", "last2")
 		  Users.insert(3, "first2", "last3")
 		  Users.insert(4, "first1", "last1")
 		  val query = for(u <- Users limit(0,2) ) yield u
 		  assertEquals(2, query.list.length)
 		  val query2 = for(u <- Users limit(0,3) ) yield u
 		  assertEquals(3, query2.list.length)
 		  val query3 = for(u <- Users orderBy(Users.id asc) limit(2,2) ) yield u
 		  assertEquals(2, query3.list.tail.head._1)
 	  }
  }
  
}
