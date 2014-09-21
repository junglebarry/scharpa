package junglebarry.scharpa

import org.scalatest._

class SimpleChartParserSpec extends FlatSpec with Matchers {

  "DCG notation" should "use ==> to encode non-terminal rules" in {
    val a: SimpleRule = 'A ==> 'B
    a should be(SimpleRule('A, 'B))

    val s: SimpleRule = 'S ==> ('NP, 'VP)
    s should be(SimpleRule('S, 'NP, 'VP))
  }

  it should "use --> to encode terminal (lexical) rules" in {
    val cheese: SimpleRule = 'Item --> "CHEESE"
    cheese should be(SimpleRule('Item, "CHEESE"))

    val montereyJack: SimpleRule = 'Item --> ("MONTEREY", "JACK")
    montereyJack should be(SimpleRule('Item, "MONTEREY", "JACK"))
  }

  "Rule.nextSymbol" should "be the first symbol on the RHS" in new SimpleGrammar {
    the.nextSymbol should be(Some(Leaf("the")))
    cat.nextSymbol should be(Some(Leaf("cat")))
    cat.nextSymbol should not be (Some(NonTerm('cat)))
    s1.nextSymbol should be(Some(NonTerm('NP)))
  }

  "Rule.nextSymbolIs" should "be true only for the first symbol on the RHS" in new SimpleGrammar {
    the.nextSymbolIs("the") should be(true)
    the.nextSymbolIs("cat") should be(false)
    s1.nextSymbolIs('NP) should be(true)
    s1.nextSymbolIs('S) should be(false)
  }

  "WordArc" should "always be passive" in {
    WordArc(0, 1, "the").active should be(false)
  }

  it should "not be extensible through the fundamental rule" in {
    WordArc(0, 1, "the").applyFundamental(WordArc(1, 2, "cat")) should be(None)
  }

  "RuleArc.active" should "be true when the rule has more RHS nodes to process" in new SimpleGrammar {
    RuleArc(0, 0, RuleApplication(the, Seq.empty[Arc], 0)).active should be(true)
    RuleArc(0, 0, RuleApplication(s2, Seq.empty[Arc], 0)).active should be(true)
    RuleArc(0, 1, RuleApplication(s2, Seq.empty[Arc], 1)).active should be(true)
    RuleArc(0, 2, RuleApplication(s2, Seq.empty[Arc], 2)).active should be(true)
  }

  it should "be false when the rule has no more RHS nodes to process" in new SimpleGrammar {
    RuleArc(0, 1, RuleApplication(the, Seq.empty[Arc], 1)).active should be(false)
    RuleArc(0, 3, RuleApplication(s2, Seq.empty[Arc], 3)).active should be(false)
  }

  "RuleArc.applyFundamental" should "return None when the node is passive (it cannot be extended)" in new SimpleGrammar {
    RuleArc(0, 1, RuleApplication(the, Seq.empty[Arc], 1)).applyFundamental(WordArc(0, 1, "the")) should be(None)
  }

  it should "return None when the hypothesised arc does not occur in the next position" in new SimpleGrammar {
    RuleArc(0, 0, RuleApplication(the, Seq.empty[Arc], 0)).applyFundamental(WordArc(1, 2, "the")) should be(None)
  }

  it should "return None when the hypothesised arc does not have a matching symbol" in new SimpleGrammar {
    RuleArc(0, 0, RuleApplication(the, Seq.empty[Arc], 0)).applyFundamental(WordArc(0, 1, "cat")) should be(None)
  }

  it should "return an extended arc when the conditions are met (right symbol, right index)" in new SimpleGrammar {
    RuleArc(0, 0, RuleApplication(the, Seq.empty[Arc], 0)).applyFundamental(wordThe) should be(Some(RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))))
  }

  "BreadthFirstChartParser.nextAgenda" should "behave like a queue" in {
    val parser = new BFBUChartParser {}
    val a = WordArc(0, 1, "a")
    val b = WordArc(1, 2, "b")
    val c = WordArc(2, 3, "c")
    val d = WordArc(3, 4, "d")
    parser.nextAgenda(Seq(a, b), Seq(c, d)) should be(Seq(a, b, c, d))
  }

  "DepthFirstChartParser.nextAgenda" should "behave like a stack" in {
    val parser = new DFTDChartParser {}
    val a = WordArc(0, 1, "a")
    val b = WordArc(1, 2, "b")
    val c = WordArc(2, 3, "c")
    val d = WordArc(3, 4, "d")
    parser.nextAgenda(Seq(a, b), Seq(c, d)) should be(Seq(c, d, a, b))
  }

  "BFBUParser.parse" should "contain the original word arcs" in new TheCatSatBFBU {
    chart.arcs should (
      be('nonEmpty) and
      contain(wordThe) and
      contain(wordCat) and
      contain(wordSat)
    )
  }

  it should "contain fully-applied rules (passive)" in new TheCatSatBFBU {
    val arcThe = RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))
    val arcCat = RuleArc(1, 2, RuleApplication(cat, Seq(wordCat), 1))
    val arcSat = RuleArc(2, 3, RuleApplication(sat, Seq(wordSat), 1))
    val arcNp = RuleArc(0, 2, RuleApplication(np, Seq(arcThe, arcCat), 2))
    val arcVp = RuleArc(2, 3, RuleApplication(ivp, Seq(arcSat), 1))
    val arcS1 = RuleArc(0, 3, RuleApplication(s1, Seq(arcNp, arcVp), 2))

    chart.arcs should (
      be('nonEmpty) and
      contain(arcThe) and
      contain(arcCat) and
      contain(arcSat) and
      contain(arcNp) and
      contain(arcVp) and
      contain(arcS1) and
      contain(RuleArc(0, 3, RuleApplication(top, Seq(arcS1), 1)))
    )
  }

  it should "contain partially-applied rules (memoised)" in new TheCatSatBFBU {
    val arcThe = RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))
    val arcCat = RuleArc(1, 2, RuleApplication(cat, Seq(wordCat), 1))
    val arcSat = RuleArc(2, 3, RuleApplication(sat, Seq(wordSat), 1))
    val arcNp = RuleArc(0, 2, RuleApplication(np, Seq(arcThe, arcCat), 2))
    val arcVp = RuleArc(2, 3, RuleApplication(ivp, Seq(arcSat), 1))

    chart.arcs should (
      be('nonEmpty) and
      contain(RuleArc(0, 1, RuleApplication(np, Seq(arcThe), 1))) and
      contain(RuleArc(2, 3, RuleApplication(tvp, Seq(arcSat), 1))) and
      contain(RuleArc(0, 2, RuleApplication(s1, Seq(arcNp), 1))) and
      contain(RuleArc(0, 2, RuleApplication(s2, Seq(arcNp), 1))) and
      contain(RuleArc(0, 3, RuleApplication(s2, Seq(arcNp, arcVp), 2)))
    )
  }

  it should "deal with multiword expressions in the lexicon" in new SimpleGrammar with BottomUpBreadthFirst {
    val chart: Chart = parser.parse(grammar)(Seq("the", "sheep", "dog", "sat"))

    val wordSheep = WordArc(1, 2, "sheep")
    val wordDog = WordArc(2, 3, "dog")
    override val wordSat = WordArc(3, 4, "sat")
    chart.arcs should (
      be('nonEmpty) and
      contain(wordThe) and
      contain(wordSheep) and
      contain(wordDog) and
      contain(wordSat)
    )

    val arcThe = RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))
    val arcSheepdog = RuleArc(1, 3, RuleApplication(sheepdog, Seq(wordSheep, wordDog), 2))
    val arcSat = RuleArc(3, 4, RuleApplication(sat, Seq(wordSat), 1))
    val arcNp = RuleArc(0, 3, RuleApplication(np, Seq(arcThe, arcSheepdog), 2))
    val arcVp = RuleArc(3, 4, RuleApplication(ivp, Seq(arcSat), 1))
    val arcS1 = RuleArc(0, 4, RuleApplication(s1, Seq(arcNp, arcVp), 2))

    chart.arcs should (
      contain(arcThe) and
      contain(arcSheepdog) and
      contain(arcSat) and
      contain(arcNp) and
      contain(arcVp) and
      contain(arcS1) and
      contain(RuleArc(0, 4, RuleApplication(top, Seq(arcS1), 1)))
    )
  }

  it should "deal with unknown words, by allowing parsing to continue" in new SimpleGrammar with BottomUpBreadthFirst {
    val chart: Chart = parser.parse(grammar)(Seq("the", "sheep", "shamen", "sat"))

    override val wordThe = WordArc(0, 1, "the")
    val wordSheep = WordArc(1, 2, "sheep")
    val wordShamen = WordArc(2, 3, "shamen")
    override val wordSat = WordArc(3, 4, "sat")

    chart.arcs should (
      be('nonEmpty) and
      contain(wordThe) and
      contain(wordSheep) and
      contain(wordShamen) and
      contain(wordSat)
    )

    val arcThe = RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))
    val arcSheep = RuleArc(1, 2, RuleApplication(sheep, Seq(wordSheep), 1))
    val arcSat = RuleArc(3, 4, RuleApplication(sat, Seq(wordSat), 1))
    val arcNp = RuleArc(0, 2, RuleApplication(np, Seq(arcThe, arcSheep), 2))
    val arcVp = RuleArc(3, 4, RuleApplication(ivp, Seq(arcSat), 1))
    val arcS1 = RuleArc(0, 4, RuleApplication(s1, Seq(arcNp, arcVp), 2))

    chart.arcs should (
      contain(arcThe) and
      contain(arcSheep) and
      contain(arcSat) and
      // partial NP matched
      contain(arcNp) and
      // intransitive VP matched, despite preceding unknown word
      contain(arcVp) and
      // no sentence matched
      not contain (arcS1) and
      not contain (RuleArc(0, 4, RuleApplication(top, Seq(arcS1), 1)))
    )
  }

  "DFTDParser.parse" should "contain the original word arcs" in new TheCatSatDFTD {
    chart.arcs should (
      be('nonEmpty) and
      contain(wordThe) and
      contain(wordCat) and
      contain(wordSat)
    )
  }

  it should "contain fully-applied rules (passive)" in new TheCatSatDFTD {
    val arcThe = RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))
    val arcCat = RuleArc(1, 2, RuleApplication(cat, Seq(wordCat), 1))
    val arcSat = RuleArc(2, 3, RuleApplication(sat, Seq(wordSat), 1))
    val arcNp = RuleArc(0, 2, RuleApplication(np, Seq(arcThe, arcCat), 2))
    val arcVp = RuleArc(2, 3, RuleApplication(ivp, Seq(arcSat), 1))
    val arcS1 = RuleArc(0, 3, RuleApplication(s1, Seq(arcNp, arcVp), 2))
    chart.arcs should (
      be('nonEmpty) and
      contain(arcThe) and
      contain(arcCat) and
      contain(arcSat) and
      contain(arcNp) and
      contain(arcVp) and
      contain(arcS1) and
      contain(RuleArc(0, 3, RuleApplication(top, Seq(arcS1), 1)))
    )
  }

  it should "contain partially-applied rules (memoised)" in new TheCatSatDFTD {
    val arcThe = RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))
    val arcCat = RuleArc(1, 2, RuleApplication(cat, Seq(wordCat), 1))
    val arcSat = RuleArc(2, 3, RuleApplication(sat, Seq(wordSat), 1))
    val arcNp = RuleArc(0, 2, RuleApplication(np, Seq(arcThe, arcCat), 2))
    val arcVp = RuleArc(2, 3, RuleApplication(ivp, Seq(arcSat), 1))
    val arcS1 = RuleArc(0, 3, RuleApplication(s1, Seq(arcNp, arcVp), 2))
    chart.arcs should (
      be('nonEmpty) and
      contain(RuleArc(0, 1, RuleApplication(np, Seq(arcThe), 1))) and
      contain(RuleArc(2, 3, RuleApplication(tvp, Seq(arcSat), 1))) and
      contain(RuleArc(0, 2, RuleApplication(s1, Seq(arcNp), 1))) and
      contain(RuleArc(0, 2, RuleApplication(s2, Seq(arcNp), 1))) and
      contain(RuleArc(0, 3, RuleApplication(s2, Seq(arcNp, arcVp), 2)))
    )
  }

  it should "deal with multiword expressions in the lexicon" in new SimpleGrammar with TopDownDepthFirst {
    val chart: Chart = parser.parse(grammar)(Seq("the", "sheep", "dog", "sat"))

    val wordSheep = WordArc(1, 2, "sheep")
    val wordDog = WordArc(2, 3, "dog")
    override val wordSat = WordArc(3, 4, "sat")
    chart.arcs should (
      be('nonEmpty) and
      contain(wordThe) and
      contain(wordSheep) and
      contain(wordDog) and
      contain(wordSat)
    )

    val arcThe = RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))
    val arcSheepdog = RuleArc(1, 3, RuleApplication(sheepdog, Seq(wordSheep, wordDog), 2))
    val arcSat = RuleArc(3, 4, RuleApplication(sat, Seq(wordSat), 1))
    val arcNp = RuleArc(0, 3, RuleApplication(np, Seq(arcThe, arcSheepdog), 2))
    val arcVp = RuleArc(3, 4, RuleApplication(ivp, Seq(arcSat), 1))
    val arcS1 = RuleArc(0, 4, RuleApplication(s1, Seq(arcNp, arcVp), 2))

    chart.arcs should (
      contain(arcThe) and
      contain(arcSheepdog) and
      contain(arcSat) and
      contain(arcNp) and
      contain(arcVp) and
      contain(arcS1) and
      contain(RuleArc(0, 4, RuleApplication(top, Seq(arcS1), 1)))
    )
  }

  it should "not be capable of dealing with unknown words, as parsing stops" in new SimpleGrammar with TopDownDepthFirst {
    val chart: Chart = parser.parse(grammar)(Seq("the", "sheep", "shamen", "sat"))

    override val wordThe = WordArc(0, 1, "the")
    val wordSheep = WordArc(1, 2, "sheep")
    val wordShamen = WordArc(2, 3, "shamen")
    override val wordSat = WordArc(3, 4, "sat")

    chart.arcs should (
      be('nonEmpty) and
      contain(wordThe) and
      contain(wordSheep) and
      contain(wordShamen) and
      contain(wordSat)
    )

    val arcThe = RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))
    val arcSheep = RuleArc(1, 2, RuleApplication(sheep, Seq(wordSheep), 1))
    val arcSat = RuleArc(3, 4, RuleApplication(sat, Seq(wordSat), 1))
    val arcNp = RuleArc(0, 2, RuleApplication(np, Seq(arcThe, arcSheep), 2))
    val arcVp = RuleArc(3, 4, RuleApplication(ivp, Seq(arcSat), 1))
    val arcS1 = RuleArc(0, 4, RuleApplication(s1, Seq(arcNp, arcVp), 2))

    chart.arcs should (
      contain(arcThe) and
      contain(arcSheep) and
      // partial NP matched
      contain(arcNp) and
      // doesn't get to verb, VP, S, or TOP
      not contain (arcSat) and
      not contain (arcVp) and
      not contain (arcS1) and
      not contain (RuleArc(0, 4, RuleApplication(top, Seq(arcS1), 1)))
    )
  }

  "ChartParser.readout" should "return a terminal for a WordArc" in {
    ChartParser.readout(WordArc(0, 1, "the")) should be("the")
    ChartParser.readout(WordArc(1, 2, "cat")) should be("cat")
  }

  it should "return an S-expression for a simple RuleArc" in new SimpleGrammar {
    ChartParser.readout(RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))) should be("(Det the)")
    ChartParser.readout(RuleArc(1, 2, RuleApplication(cat, Seq(wordCat), 1))) should be("(N cat)")
  }

  it should "return an S-expression for a nested RuleArc" in new SimpleGrammar {
    val arcThe = RuleArc(0, 1, RuleApplication(the, Seq(wordThe), 1))
    val arcCat = RuleArc(1, 2, RuleApplication(cat, Seq(wordCat), 1))
    ChartParser.readout(RuleArc(0, 2, RuleApplication(np, Seq(arcThe, arcCat), 2))) should be("(NP (Det the) (N cat))")
  }

  "ChartParser.passiveNodes" should "return completed nodes" in new TheCatSatBFBU {
    val nodes = ChartParser.passiveNodes(chart, grammar.top.lhs)
    nodes.size should be(1)
    val RuleArc(start, end, RuleApplication(sym, _, _)) = nodes.head
    start should be(0)
    end should be(3)
    sym should be(top)
  }
}

trait BottomUpBreadthFirst {
  val parser = new BFBUChartParser {}
}

trait TopDownDepthFirst {
  val parser = new DFTDChartParser {}
}

trait SimpleGrammar {
  // words
  val wordThe = WordArc(0, 1, "the")
  val wordCat = WordArc(1, 2, "cat")
  val wordSat = WordArc(2, 3, "sat")

  // rules
  val the = 'Det --> "the"
  val cat = 'N --> "cat"
  val dog = 'N --> "dog"
  val sheep = 'N --> "sheep"
  val sheepdog = 'N --> ("sheep", "dog")
  val mat = 'N --> "mat"
  val sat = 'V --> "sat"
  val ate = 'V --> "ate"
  val on = 'P --> "on"
  val pp = 'PP ==> ('P, 'NP)
  val np = 'NP ==> ('Det, 'N)
  val tvp = 'VP ==> ('V, 'NP)
  val ivp = 'VP ==> 'V
  val s1 = 'S ==> ('NP, 'VP)
  val s2 = 'S ==> ('NP, 'VP, 'PP)

  // grammar
  val rules = Set[Rule](the, cat, dog, sheep, sheepdog, mat, sat, ate, on, pp, np, tvp, ivp, s1, s2)
  val top = TopRule('S)
  val grammar = new Grammar(rules, top)
}

trait TheCatSatBFBU extends SimpleGrammar with BottomUpBreadthFirst {
  val chart: Chart = parser.parse(grammar)(Seq("the", "cat", "sat"))
}

trait TheCatSatDFTD extends SimpleGrammar with TopDownDepthFirst {
  val chart: Chart = parser.parse(grammar)(Seq("the", "cat", "sat"))
}