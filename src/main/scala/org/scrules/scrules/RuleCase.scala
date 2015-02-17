package org.scrules.scrules


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
