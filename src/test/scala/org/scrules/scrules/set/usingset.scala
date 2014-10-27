package org.scrules.scrules.set
import org.junit._
import org.junit.Assert._
import scala.collection.immutable.TreeSet

/*
 * TODO :
 *    * Ability to obtain iterators over definition set.
 *    * Pb : there is no log/explanation about the rule that was matched.
 *    	Maybe the rulecase could also return its label as a side explaination. (also interesting for testing)
 *    * Write a Product operator that builds the cartesian product of two rulesets.
 */

abstract class ConstantPartialFunction[Input, Output](val returnValue : Output) extends PartialFunction[Input, Output] {
  def apply(v1 : Input) : Output = returnValue 
}

abstract class MatchExpr[Input] extends (Input => Boolean)

case class OrExpr[Input](val exprs: List[MatchExpr[Input]]) extends MatchExpr[Input] {
  def apply(input : Input) : Boolean = exprs.exists(_.apply(input))
}

case class AndExpr[Input](val exprs: List[MatchExpr[Input]]) extends MatchExpr[Input] {
  def apply(input : Input) : Boolean = !exprs.exists(!_.apply(input))
}

class EqExpr[Input, Value](val projection : (Input => Value), val expectedValue : Value) extends MatchExpr[Input] {
  def apply(input : Input) : Boolean = expectedValue.equals(projection.apply(input))
}

case class RuleCase[Input, Output]
			(var label : String, 
			 val matchExpr : MatchExpr[Input],
			 override val returnValue : Output,
			 val salience : Int = 0)
			 extends ConstantPartialFunction[Input, Output](returnValue) with Ordered[RuleCase[Input, Output]] {
  def isDefinedAt(x : Input) = matchExpr.apply(x)
  def compare(that: RuleCase[Input, Output]): Int = this.salience compare that.salience
  override def andThen[OtherOutput](other : Function1[Output, OtherOutput]) : RuleCase[Input, OtherOutput] = 
    new RuleCase[Input, OtherOutput](this.label, this.matchExpr, other.apply(this.returnValue), this.salience)
}


class RuleSet[Input, Output](val rules : Map[String, RuleCase[Input, Output]], val defaultResult : Output) 
						extends Function[Input, Output] {
  
  val runtime = new TreeSet[RuleCase[Input, Output]]()(new Ordering[RuleCase[Input, Output]] {
    def compare(ac1: RuleCase[Input, Output], ac2: RuleCase[Input, Output]): Int = {
      ac2.salience compare ac1.salience
    }
  }) ++ rules.values
  
  def apply(input : Input) : Output = runtime.find(_.isDefinedAt(input)) match { 
    										case Some(ruleCase) => ruleCase.returnValue
    										case None => defaultResult
    									}
  def merge(other : RuleSet[Input, Output]) : RuleSet[Input, Output] =  
		  new RuleSet[Input, Output](this.rules ++ other.rules, this.defaultResult)
		  
  override def andThen[OtherOutput](other : Function1[Output, OtherOutput]) : RuleSet[Input, OtherOutput] = {
    new RuleSet[Input, OtherOutput](
        this.rules map (x => (x._1, x._2.andThen(other))),
        other.apply(this.defaultResult))
  }
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
	  
    @Test
    def testOK() = {
		assertEquals(new MyOutput("world"), myRuleSet.apply(new MyInput(Identifier("my", "BIC"), true)))
		assertEquals(new MyOutput("bingo"), myRuleSet.apply(new MyInput(Identifier("mya", "BIC"), true)))
		assertEquals(new MyOutput("hello"), myRuleSet.apply(new MyInput(Identifier("mya", "BIC"), false)))
    }

}