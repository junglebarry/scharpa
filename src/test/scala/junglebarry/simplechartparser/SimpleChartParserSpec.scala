package junglebarry.simplechartparser

import org.scalatest._

class SimpleChartParserSpec extends FlatSpec with Matchers {

  "WordArc" should "always be passive" in {
    WordArc(0, 1, "the").active should be(false)
  }

  it should "not be extensible through the fundamental rule" in {
    WordArc(0, 1, "the").applyFundamental(WordArc(1, 2, "cat")) should be(None)
  }

  "BFBUParser.parse" should "contain the original word arcs" in new TheCatSat {
    chart.arcs should (
      be('nonEmpty) and
      contain(WordArc(0, 1, "the")) and
      contain(WordArc(1, 2, "cat")) and
      contain(WordArc(2, 3, "sat"))
    )
  }

  it should "contain fully-applied rules (passive)" in new TheCatSat {
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

  it should "contain partially-applied rules (memoised)" in new TheCatSat {
    chart.arcs should (
      be('nonEmpty) and
      contain(RuleArc(0, 1, RuleApplication(np, 1))) and
      contain(RuleArc(2, 3, RuleApplication(tvp, 1))) and
      contain(RuleArc(0, 2, RuleApplication(s1, 1))) and
      contain(RuleArc(0, 2, RuleApplication(s2, 1))) and
      contain(RuleArc(0, 3, RuleApplication(s2, 2)))
    )
  }

  it should "deal with multiword expressions in the lexicon" in new SimpleGrammar with BottomUpBreadthFirst {
    val chart: Chart = parser.parse(grammar)(Seq("the", "sheep", "dog", "sat"))

    chart.arcs should (
      be('nonEmpty) and
      contain(WordArc(0, 1, "the")) and
      contain(WordArc(1, 2, "sheep")) and
      contain(WordArc(2, 3, "dog")) and
      contain(WordArc(3, 4, "sat"))
    )

    chart.arcs should (
      contain(RuleArc(0, 1, RuleApplication(the, 1))) and
      contain(RuleArc(1, 3, RuleApplication(sheepdog, 2))) and
      contain(RuleArc(3, 4, RuleApplication(sat, 1))) and
      contain(RuleArc(0, 3, RuleApplication(np, 2))) and
      contain(RuleArc(3, 4, RuleApplication(ivp, 1))) and
      contain(RuleArc(0, 4, RuleApplication(s1, 2)))
    )
  }

  it should "deal with unknown words, by allowing parsing to continue" in new SimpleGrammar with BottomUpBreadthFirst {
    val chart: Chart = parser.parse(grammar)(Seq("the", "sheep", "shamen", "sat"))

    chart.arcs should (
      be('nonEmpty) and
      contain(WordArc(0, 1, "the")) and
      contain(WordArc(1, 2, "sheep")) and
      contain(WordArc(2, 3, "shamen")) and
      contain(WordArc(3, 4, "sat"))
    )

    chart.arcs should (
      contain(RuleArc(0, 1, RuleApplication(the, 1))) and
      contain(RuleArc(1, 2, RuleApplication(sheep, 1))) and
      contain(RuleArc(3, 4, RuleApplication(sat, 1))) and
      // partial NP matched
      contain(RuleArc(0, 2, RuleApplication(np, 2))) and
      // intransitive VP matched, despite preceding unknown word
      contain(RuleArc(3, 4, RuleApplication(ivp, 1))) and
      // no sentence matched
      not contain (RuleArc(0, 4, RuleApplication(s1, 2)))
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
  val sheep = Rule("N", "sheep")
  val sheepdog = Rule("N", "sheep", "dog")
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
  val grammar = new Grammar(Set(the, cat, dog, sheep, sheepdog, mat, sat, ate, on, pp, np, tvp, ivp, s1, s2))
}

trait TheCatSat extends SimpleGrammar with BottomUpBreadthFirst {
  val chart: Chart = parser.parse(grammar)(Seq("the", "cat", "sat"))
}