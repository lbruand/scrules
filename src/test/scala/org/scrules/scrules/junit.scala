package org.scrules.scrules

import org.junit._
import Assert._

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe=>ru}


// TODO union, compose on a RuleSet.^M
// What is a RuleSet : It is a piece-wise definition of a Function from one Typ
// We can further restrict the input and output type to enumerations and compos

class Rule(val name: String)
  extends annotation.StaticAnnotation
  
abstract class Matcher[Input] extends (Input => Boolean)

case class RuleFunction/*[Input, Output]*/(
    var label: String = "")
    /* /*,
    val matcher : (Input => Boolean),
    val result : Output,
    val salience : Int = 0*/*/

class RuleSetBase {
  @Rule("rule0")
  def rule0 : RuleFunction = new RuleFunction()
  @Rule("rule1")
  def rule1 : RuleFunction = new RuleFunction()
}
class RuleSet extends RuleSetBase {
	@Rule("rule1")
    override def rule1 : RuleFunction = new RuleFunction()
	
	@Rule("rule2")
    def rule2 : RuleFunction = new RuleFunction()
}

object Rule {
  
  def listProperties[T: TypeTag]: List[(TermSymbol, Annotation)] = {
	  // a field is a Term that is a Var or a Val
	  val fields = typeOf[T].members.collect{ case s: TermSymbol => s }.
	    filter(s => s.isMethod)
	
	  // then only keep the ones with a MyProperty annotation
	  fields.flatMap(f => f.annotations.find(_.tpe =:= typeOf[Rule]).
	    map((f, _))).toList
	}
  
  def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T]
  def extractRules(ruleSet : RuleSet) : List[RuleFunction] = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(ruleSet)
    val tpe = getTypeTag(ruleSet).tpe
    tpe.members.flatMap(symbol => {
      if (symbol.isMethod && symbol.asMethod.returnType.equals(typeOf[RuleFunction])) {
	    val methodMirror = im.reflectMethod(symbol.asMethod)
	    val name : String= symbol.asMethod.fullName
	    val rule : RuleFunction = methodMirror.apply().asInstanceOf[RuleFunction]
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
