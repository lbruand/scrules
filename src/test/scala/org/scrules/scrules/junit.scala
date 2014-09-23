package org.scrules.scrules

import org.junit._
import Assert._

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe=>ru}

abstract class Matcher[Input] extends (Input => Boolean)

case class Rule/*[Input, Output]*/(
    var label: String = "" /*,
    val matcher : (Input => Boolean),
    val result : Output,
    val salience : Int = 0*/)

class RuleSetBase {
  def rule0 : Rule = new Rule()
  def rule1 : Rule = new Rule()
}
class RuleSet extends RuleSetBase {
    override def rule1 : Rule = new Rule()
    def rule2 : Rule = new Rule()
}

object Rule {
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]
  def extractRules(ruleSet : RuleSet) : List[Rule] = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(ruleSet)
    val tpe = getTypeTag(ruleSet).tpe
    tpe.members.flatMap(symbol => {
      if (symbol.isMethod && symbol.asMethod.returnType.equals(typeOf[Rule])) {
	    val methodMirror = im.reflectMethod(symbol.asMethod)
	    val name : String= symbol.asMethod.fullName
	    val rule : Rule = methodMirror.apply().asInstanceOf[Rule]
	    rule.label = name
	    Some(rule)
      } else {
    	None
      }
    }).toList
    
  }
}


@Test
class MyTest {

    @Test
    def testOK() = {
      System.out.println(Rule.extractRules(new RuleSet()))
      assertTrue(true)
    }

}
