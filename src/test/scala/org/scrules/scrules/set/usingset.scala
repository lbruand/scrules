package org.scrules.scrules.set
import org.junit._
import org.junit.Assert._
import scala.collection.immutable.TreeSet



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
}

class RuleSet[Input, Output](val rules : Map[String, RuleCase[Input, Output]], val defaultResult : Output) extends Function[Input, Output] {
  val runtime = rules.values.toSet
  def apply(input : Input) : Output = runtime.find(_.isDefinedAt(input)) match { 
    										case Some(ruleCase) => ruleCase.returnValue
    										case None => defaultResult
    									}
//new TreeSet(rules.values)
}

// ===== Test context =========

case class Identifier(val identifier : String, val identifierType : String)
case class MyInput(val id : Identifier, val b : Boolean)
case class MyOutput(val result : String)

@Test
class OtherTest {
	val myRuleSet = new RuleSet[MyInput, MyOutput](rules = 
	  Map(
	    "rule1" -> new RuleCase("", new EqExpr(_.id.identifier, "my"), new MyOutput("world"))
	  ), MyOutput("hello"))
	  
    @Test
    def testOK() = {
		assertEquals(new MyOutput("world"), myRuleSet.apply(new MyInput(Identifier("my", "BIC"), true)))
		assertEquals(new MyOutput("hello"), myRuleSet.apply(new MyInput(Identifier("mya", "BIC"), true)))
    }

}