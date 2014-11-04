package org.scrules.scrules.set
import org.junit._
import org.junit.Assert._
import scala.collection.immutable.TreeSet

/*
 * TODO :
 *    * Ability to obtain iterators over definition set.
 *    * Pb : there is no log/explanation about the rule that was matched.
 *    	Maybe the rulecase could also return its label as a side explaination. (also interesting for testing)
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

case class RuleCase[Input, Output]
			(var label : String, 
			 val matchExpr : MatchExpr[Input],
			 override val returnValue : Output,
			 val salience : Int = 0)
			 extends ConstantPartialFunction[Input, Output](returnValue) with Ordered[RuleCase[Input, Output]] {
  def isDefinedAt(x : Input) = matchExpr.apply(x)
  
  def compare(that: RuleCase[Input, Output]): Int = this.salience compare that.salience // TODO Maybe use the label to totalise the order.
  
  override def andThen[OtherOutput](other : Function1[Output, OtherOutput]) : RuleCase[Input, OtherOutput] = 
    new RuleCase[Input, OtherOutput](this.label, this.matchExpr, other.apply(this.returnValue), this.salience)

  override def compose[PreInput](g : PreInput => Input) : RuleCase[PreInput, Output] = new RuleCase[PreInput, Output](this.label,
		  																this.matchExpr.compose(g), this.returnValue, this.salience)		  																
}


class RuleSet[Input, Output](val rules : Map[String, RuleCase[Input, Output]], val defaultResult : Output) 
						extends Function[Input, Output] {
  
  val runtime = new TreeSet[RuleCase[Input, Output]]()(new Ordering[RuleCase[Input, Output]] {
    def compare(ac1: RuleCase[Input, Output], ac2: RuleCase[Input, Output]): Int = {
      ac2.salience compare ac1.salience
    }
  }) ++ rules.values
  
  def findRule(input : Input) : Option[RuleCase[Input, Output]] = runtime.find(_.isDefinedAt(input))
  
  def apply(input : Input) : Output = findRule(input) match { 
    										case Some(ruleCase) => ruleCase.returnValue
    										case None => defaultResult
    									}
  
  def merge(other : RuleSet[Input, Output]) : RuleSet[Input, Output] =
		  new RuleSet[Input, Output](this.rules ++ other.rules, this.defaultResult)

  override def andThen[OtherOutput](other : Output => OtherOutput) : RuleSet[Input, OtherOutput] = {
    new RuleSet[Input, OtherOutput](
        this.rules map (x => (x._1, x._2.andThen(other))),
        other.apply(this.defaultResult))
  }
  
  override def compose[PreInput](g : PreInput => Input) : RuleSet[PreInput, Output] =
    new RuleSet[PreInput, Output](
        this.rules map (x => (x._1, x._2.compose(g))),
        this.defaultResult
    )

  def crossProduct[SecondInput, SecondOutput]( second : RuleSet[SecondInput, SecondOutput]) : 
		  														  RuleSet[(Input, SecondInput), (Output, SecondOutput)] = 
		  														    new RuleSet[(Input, SecondInput), (Output, SecondOutput)](
		  														    			for { firstRuleCase <- this.rules; secondRuleCase <- second.rules } yield ("<"+firstRuleCase._2.label + ":" + secondRuleCase._2.label +">",
		  														    			    new RuleCase[(Input, SecondInput), (Output, SecondOutput)](
		  														    			    label = "<"+firstRuleCase._2.label + ":" + secondRuleCase._2.label +">",
		  														    			    matchExpr = new AndExpr[(Input, SecondInput)](List(firstRuleCase._2.matchExpr.compose(_._1), secondRuleCase._2.matchExpr.compose(_._2))),
		  														    			    returnValue = (firstRuleCase._2.returnValue, secondRuleCase._2.returnValue),
		  														    			    salience = firstRuleCase._2.salience * secondRuleCase._2.salience /* TODO */)),

		  														    			(this.defaultResult, second.defaultResult))
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