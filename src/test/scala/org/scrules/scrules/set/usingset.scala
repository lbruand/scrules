package org.scrules.scrules.set
import org.junit._
import org.junit.Assert._
import scala.collection.immutable.TreeSet
import org.slf4j.LoggerFactory
import org.scrules.scrules.RuleSet
import org.scrules.scrules.RuleCase


/*
 * TODO :
 *    * Ability to obtain iterators over definition set.
 *    * Pb : there is no log/explanation about the rule that was matched.
 *    * Write a union and a difference operator.
 */

abstract class ConstantPartialFunction[Input, Output](val returnValue : Output) extends PartialFunction[Input, Output] {
  def apply(v1 : Input) : Output = returnValue 
}

sealed abstract class MatchExpr[Input] extends (Input => Boolean) {
  override def compose[A](g : A => Input) : MatchExpr[A] = null
}

case class OrExpr[Input](val exprs: List[MatchExpr[Input]]) extends MatchExpr[Input] {
  def apply(input : Input) : Boolean = exprs.exists(_.apply(input))
  override def compose[A](g : A => Input) : MatchExpr[A] = new OrExpr(exprs map (_.compose(g).asInstanceOf[MatchExpr[A]])) 
}

case class AndExpr[Input](val exprs: List[MatchExpr[Input]]) extends MatchExpr[Input] {
  def apply(input : Input) : Boolean = !exprs.exists(!_.apply(input))
  override def compose[A](g : A => Input) : MatchExpr[A] = new AndExpr(exprs map (_.compose(g).asInstanceOf[MatchExpr[A]]))
}

// TODO Maybe use the Lenses here.
case class EqExpr[Input, Value](val projection : (Input => Value), val expectedValue : Value) extends MatchExpr[Input] {
  def apply(input : Input) : Boolean = expectedValue.equals(projection.apply(input))
  
  override def compose[A](g : A => Input) : MatchExpr[A] = new EqExpr(projection.compose(g), expectedValue)
}


// ===== Test context =========

case class Identifier(val identifier : String, val identifierType : String)
case class MyInput(val id : Identifier, val b : Boolean)
case class MyOutput(val result : String)

@Test
class OtherTest {
	val myRuleSet = new RuleSet[MyInput, MyOutput](rules = 
	  Map(
	    "rule1" -> new RuleCase("rule1", new EqExpr(_.id.identifier, "my"), new MyOutput("world"), salience=1),
	    "rule2" -> new RuleCase("rule2", new EqExpr(_.b, true), new MyOutput("bingo"))
	  ), MyOutput("hello"))
	  
	val otherRuleSet = new RuleSet[MyInput, MyOutput](rules = 
	  Map(
	    "otherrule1" -> new RuleCase("otherrule1", new EqExpr(_.id.identifier, "my"), new MyOutput("otherworld"), salience=1),
	    "otherrule2" -> new RuleCase("otherrule2", new EqExpr(_.b, true), new MyOutput("bingo"))
	  ), MyOutput("hello"))
	  
    @Test
    def testApply() = {
		assertEquals(new MyOutput("world"), myRuleSet.apply(new MyInput(Identifier("my", "BIC"), true)))
		assertEquals(new MyOutput("bingo"), myRuleSet.apply(new MyInput(Identifier("mya", "BIC"), true)))
		assertEquals(new MyOutput("hello"), myRuleSet.apply(new MyInput(Identifier("mya", "BIC"), false)))
    }
	
	@Test
	def testCrossProduct() = {
	  val crossProduct = myRuleSet.crossProduct(otherRuleSet)
	  assertEquals((new MyOutput("world"), new MyOutput("otherworld")), crossProduct.apply( (new MyInput(Identifier("my", "BIC"), true), new MyInput(Identifier("my", "BIC"), true))))
	}
	
	@Test
	def testAndthen() = {
	  val myRuleSetMod = myRuleSet.andThen(_.result)
	  assertEquals("world", myRuleSetMod.apply(new MyInput(Identifier("my", "BIC"), true)))
	  assertEquals("bingo", myRuleSetMod.apply(new MyInput(Identifier("mya", "BIC"), true)))
	  assertEquals("hello", myRuleSetMod.apply(new MyInput(Identifier("mya", "BIC"), false)))
	}

}