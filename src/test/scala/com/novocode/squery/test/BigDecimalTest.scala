package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.basic.BasicDriver.Implicit._
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession
import java.math.{BigDecimal=>BigDec}

object BigDecimalTest { def main(args: Array[String]) = new BigDecimalTest().test() }

class BigDecimalTest {
  case class Money(id: Int, amount: BigDec, part: Int, x: Float)

  object Money_ extends Table[(Int, BigDec, Int, Float)]("money") {
    def id = column[Int]("id", O AutoInc, O NotNull)
    def amount = column[BigDec]("amount", O NotNull)
    def part = column[Int]("part", O NotNull)
    def x = column[Float]("x", O NotNull)
    def * = id ~ amount ~ part ~ x
  }

  val mySequence = Sequence[Int]("mysequence") start 200 inc 10

  @Test def test() {
    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {

      Money_.createTable
      mySequence.createSequence
      val ins1 = (Money_.amount ~ Money_.part ~ Money_.x).insert(new BigDec(34.34), 100, 0.0f)  
      println("BigDecimal test")
      val q1 = for(u <- Money_) yield u.id ~ u.amount ~ u.part
      println("q1: " + q1.selectStatement)
      val q2 = for(u <- Money_ ) yield u.amount.sum
      println("q2: " + q2.selectStatement)
      val q3 = for(u <- Money_ if (u.amount is new BigDec(20.0))) yield u.amount.sum
      println("q3: " + q3.selectStatement)
      val q4 = for(u <- Money_ ) yield u.x*u.part
      println("q4: " + q4.selectStatement)
      val q5 = for(u <- Money_ ) yield (u.amount*u.part).sum
      println("q5: " + q5.selectStatement)
      val q6 = for(u <- Money_ ) yield (u.amount*u.id).sum
      println("q6: " + q6.selectStatement)
      for(t <- q1) println("User tuple: "+t)
    }
  }
}
