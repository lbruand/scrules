package org.scrules.scrules

import org.junit._
import Assert._

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe=>ru}


// TODO union, compose on a RuleSet.^M
// What is a RuleSet : It is a piece-wise definition of a Function from one Typ
// We can further restrict the input and output type to enumerations and compos

class Rule
  extends annotation.StaticAnnotation
  
abstract class Matcher[Input] extends (Input => Boolean)

case class RuleCase/*[Input, Output]*/(
    var label: String = "")
    /* /*,
    val matcher : (Input => Boolean),
    val result : Output,
    val salience : Int = 0*/*/
    
class RuleSet {

}
class RuleSetBase extends RuleSet {
  @Rule
  def rule0 : RuleCase = new RuleCase()
  @Rule
  def rule1 : RuleCase = new RuleCase()
}
class RuleSetExtended extends RuleSetBase {
	@Rule
    override def rule1 : RuleCase = new RuleCase()
	
	@Rule
    def rule2 : RuleCase = new RuleCase()
}


object RuleCompiler {
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]
  def extractRules(ruleSet : RuleSetExtended) : List[RuleCase] = {
	val m = ru.runtimeMirror(getClass.getClassLoader)
	val im = m.reflect(ruleSet)
	val tpe = getTypeTag(ruleSet).tpe
	tpe.members.filter(_.isMethod).filter(_.annotations.exists(_.tpe =:= typeOf[Rule])).flatMap(symbol => {
	    val method = symbol.asMethod 
		if (method.returnType.equals(typeOf[RuleCase])) {
			val methodMirror = im.reflectMethod(method)
			val name : String= symbol.asMethod.fullName
			val rule : RuleCase = methodMirror.apply().asInstanceOf[RuleCase]
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
      assertEquals(3, RuleCompiler.extractRules(new RuleSetExtended()).size)
    }

}
