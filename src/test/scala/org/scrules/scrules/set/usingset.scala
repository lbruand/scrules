package org.scrules.scrules.set
import org.junit._
import org.junit.Assert._
import scala.collection.immutable.TreeSet
import org.slf4j.LoggerFactory
import org.scrules.scrules.RuleSet
import org.scrules.scrules.RuleCase
import org.scrules.scrules.EqExpr


/*
 * TODO :
 *    * Ability to obtain iterators over definition set.
 *    * Write a union and a difference operator.
 */




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