package org.scrules.scrules

import org.scrules.scrules.set.ConstantPartialFunction
import org.scrules.scrules.set.MatchExpr

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
