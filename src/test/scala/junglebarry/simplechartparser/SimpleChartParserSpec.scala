package junglebarry.simplechartparser

import org.scalatest._

class SimpleChartParserSpec extends FlatSpec with Matchers {

  "BFBUParser.parse" should "contain the original word arcs" in new SimpleGrammar with BottomUpBreadthFirst {
    val chart: Chart = parser.parse(grammar)(Seq("the", "cat", "sat"))

    chart.arcs should (
      be('nonEmpty) and
      contain(WordArc(0, 1, "the")) and
      contain(WordArc(1, 2, "cat")) and
      contain(WordArc(2, 3, "sat"))
    )
  }

  it should "contain fully-applied rules (passive)" in new SimpleGrammar with BottomUpBreadthFirst {
    val chart: Chart = parser.parse(grammar)(Seq("the", "cat", "sat"))

    chart.arcs should (
      be('nonEmpty) and
      contain(RuleArc(0, 1, RuleApplication(the, 1))) and
      contain(RuleArc(1, 2, RuleApplication(cat, 1))) and
      contain(RuleArc(2, 3, RuleApplication(sat, 1))) and
      contain(RuleArc(0, 2, RuleApplication(np, 2))) and
      contain(RuleArc(2, 3, RuleApplication(ivp, 1))) and
      contain(RuleArc(0, 3, RuleApplication(s1, 2)))
    )
  }

  it should "contain partially-applied rules (memoised)" in new SimpleGrammar with BottomUpBreadthFirst {
    val chart: Chart = parser.parse(grammar)(Seq("the", "cat", "sat"))

    chart.arcs should (
      be('nonEmpty) and
      contain(RuleArc(0, 1, RuleApplication(np, 1))) and
      contain(RuleArc(2, 3, RuleApplication(tvp, 1))) and
      contain(RuleArc(0, 2, RuleApplication(s1, 1))) and
      contain(RuleArc(0, 2, RuleApplication(s2, 1))) and
      contain(RuleArc(0, 3, RuleApplication(s2, 2)))
    )
  }
}

trait BottomUpBreadthFirst {
  val parser = new BFBUChartParser {}
}

trait SimpleGrammar {
  val the = Rule("Det", "the")
  val cat = Rule("N", "cat")
  val dog = Rule("N", "dog")
  val mat = Rule("N", "mat")
  val sat = Rule("V", "sat")
  val ate = Rule("V", "ate")
  val on = Rule("P", "on")
  val pp = Rule("PP", "P", "NP")
  val np = Rule("NP", "Det", "N")
  val tvp = Rule("VP", "V", "NP")
  val ivp = Rule("VP", "V")
  val s1 = Rule("S", "NP", "VP")
  val s2 = Rule("S", "NP", "VP", "PP")
  val grammar = new Grammar(Set(the, cat, dog, mat, sat, ate, on, pp, np, tvp, ivp, s1, s2))
}