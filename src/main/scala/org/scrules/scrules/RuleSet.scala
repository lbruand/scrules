package org.scrules.scrules

import org.slf4j.LoggerFactory
import scala.collection.immutable.TreeSet
import org.scrules.scrules.RuleCase

case class RuleSet[Input, Output](val rules : Map[String, RuleCase[Input, Output]], val defaultResult : Output) 
						extends Function[Input, Output] {
  def logger = LoggerFactory.getLogger(classOf[RuleSet[Input,Output]])
  
  lazy val runtime = new TreeSet[RuleCase[Input, Output]]()(new Ordering[RuleCase[Input, Output]] {
    def compare(ac1: RuleCase[Input, Output], ac2: RuleCase[Input, Output]): Int = {
      ac2.salience compare ac1.salience
    }
  }) ++ rules.values
  
  def findRule(input : Input) : Option[RuleCase[Input, Output]] = runtime.find(_.isDefinedAt(input))
  
  def apply(input : Input) : Output = findRule(input) match { 
    										case Some(ruleCase) => {
    										  if (logger.isDebugEnabled()) {
    										    logger.debug(s"rule [${ruleCase.label}] fired on input [${input}]")
    										  }
    										  ruleCase.returnValue
    										}
    										case None => defaultResult
    									}
  
  def merge(other : RuleSet[Input, Output]) : RuleSet[Input, Output] =
		  new RuleSet[Input, Output](this.rules ++ other.rules, this.defaultResult)
		  
  def modifySalience(modifySalience : Int => Int) : RuleSet[Input, Output] = 
    	new RuleSet[Input, Output](this.rules map (x => (x._1, x._2.copy(salience = modifySalience(x._2.salience)))), 
    								defaultResult)

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
		  														    			    salience = firstRuleCase._2.salience * secondRuleCase._2.salience /* TODO is it a good idea to multiply */)),

		  														    			(this.defaultResult, second.defaultResult))
}
