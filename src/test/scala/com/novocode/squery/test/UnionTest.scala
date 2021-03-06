package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.basic.BasicDriver.Implicit._
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession

object UnionTest { def main(args: Array[String]) = new UnionTest().test() }

class UnionTest {

  object Managers extends Table[(Int, String, String)]("managers") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def department = column[String]("department")
    def * = id ~ name ~ department
  }

  object Employees extends Table[(Int, String, Int)]("employees") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def manager = column[Int]("manager", O.NotNull)
    def * = id ~ name ~ manager

    // A convenience method for selecting employees by department
    def departmentIs(dept: String) = manager in Managers.where(_.department is dept).map(_.id)
  }

  @Test def test() {
    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {

      Managers.createTable
      Employees.createTable

      Managers.insertAll(
        (1, "Peter", "HR"),
        (2, "Amy", "IT"),
        (3, "Steve", "IT")
      )

      Employees.insertAll(
        (4, "Jennifer", 1),
        (5, "Tom", 1),
        (6, "Leonard", 2),
        (7, "Ben", 2),
        (8, "Greg", 3)
      )

      val q1 = for(m <- Managers where { _.department is "IT" }) yield m.id ~ m.name
      println("Managers in IT: "+ q1.selectStatement)
      q1.foreach(o => println("  "+o))

      val q2 = for(e <- Employees where { _.departmentIs("IT") }) yield e.id ~ e.name
      println("Employees in IT: " + q2.selectStatement)
      q2.foreach(o => println("  "+o))

      val q3 = for(x <- q1 union q2; _ <- Query.orderBy(x._2 asc)) yield x
      q3.dump("q3: ")
      println()
      println("Combined and sorted: " + q3.selectStatement)
      q3.foreach(o => println("  "+o))

      assertEquals(q3.list, List((2,"Amy"), (7,"Ben"), (8,"Greg"), (6,"Leonard"), (3,"Steve")))
    }
  }
}
