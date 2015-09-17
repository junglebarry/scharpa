package junglebarry

import scala.annotation.tailrec

package object scharpa {

  // Sym represents a symbol in the grammar, either a leaf or a non-terminal
  sealed trait Sym {}
  case class Leaf(symbol: String) extends Sym {
    override def toString = symbol
  }
  case class NonTerm(symbol: Symbol) extends Sym {
    override def toString = symbol.name

    /**
     * DCG notation for terminal (lexical) rules
     */
    def -->(rhsHead: String, rhs: String*): SimpleRule = SimpleRule(this, rhsHead, rhs.map(string2leaf): _*)
    /**
     * DCG notation for non-terminal rules
     */
    def ==>(rhsHead: Symbol, rhs: Symbol*): SimpleRule = SimpleRule(this, rhsHead, rhs.map(sym2nonTerm): _*)
  }

  // implicits for making rule forms easier
  implicit def string2leaf(string: String): Leaf = Leaf(string)
  implicit def sym2nonTerm(symbol: Symbol): NonTerm = NonTerm(symbol)

  trait HasNextSymbol {
    def nextSymbolIs(symbol: Sym): Boolean
  }

  trait HasRHS extends HasNextSymbol {
    def nextSymbol: Option[Sym]

    override def nextSymbolIs(symbol: Sym): Boolean = { nextSymbol.contains(symbol) }
  }

  sealed trait Rule extends HasRHS {
    def lhs: NonTerm
    def rhs: Seq[Sym]
  }

  /**
   * The TopRule is a single rule singled out as the topmost node of the grammar.
   */
  case class TopRule(rhsOnly: NonTerm, topSymbol: NonTerm = 'ROOT) extends Rule {
    override val lhs = topSymbol
    val rhs: Seq[Sym] = Seq(rhsOnly)
    override val nextSymbol: Option[Sym] = Some(rhsOnly)
  }

  /**
   * Rules are context-free grammar rules, which have a single left-hand-side entry,
   * and expand to one or more right-hand-side entries (enforced by type).
   */
  case class SimpleRule(lhs: NonTerm, rhsHead: Sym, rhsTail: Sym*) extends Rule {
    val rhs: Seq[Sym] = rhsHead +: rhsTail.toSeq
    override val nextSymbol: Option[Sym] = Some(rhsHead)
  }

  /** Generator for Rules from data, so the grammar can contain patterns for generating rules **/
  trait RuleGenerator extends HasNextSymbol {
    def ruleFor(sym: Sym): Rule
  }

  /**
   * Simple RuleGenerator based on a regular expression, which, if it matches, generates
   * a SimpleRule with a given LHS and the matching Sym as the sole RHS member.
   */
  case class RegexRuleGenerator(lhs: NonTerm, regex: String) extends RuleGenerator {
    private lazy val Rx = regex.r
    override def nextSymbolIs(symbol: Sym): Boolean = symbol match {
      case Leaf(Rx(_*)) => true
      case _ => false
    }
    override def ruleFor(sym: Sym): Rule = SimpleRule(lhs, sym)
  }

  /** A grammar is a set of rules, with an optional Top rule (for top-down traversal) **/
  case class Grammar(ruleset: Set[Rule], top: TopRule, generators: Set[RuleGenerator] = Set.empty[RuleGenerator]) {
    lazy val rules: Set[Rule] = ruleset + top
    lazy val rulesByLhs: Map[Sym, Set[Rule]] = rules.groupBy(_.lhs)
    lazy val rulesByFirstRhs: Map[Sym, Set[Rule]] = rules.filter(_.nextSymbol.isDefined).groupBy(_.nextSymbol.get)

    private lazy val generatorSeq: Seq[RuleGenerator] = generators.toSeq

    def applyGenerators(sym: Sym): Set[Rule] = {
      generatorSeq.withFilter(_.nextSymbolIs(sym)).map(_.ruleFor(sym))(collection.breakOut)
    }

    /** Find rules that start with a given symbol **/
    def rulesWhoseRhsStartsWith(sym: Sym): Set[Rule] = {
      val cached = rulesByFirstRhs.get(sym)
      val generated = applyGenerators(sym)
      cached.getOrElse(Set.empty[Rule]) ++ generated
    }

    def rulesThatExpand(sym: Sym): Set[Rule] = rulesByLhs.getOrElse(sym, Set.empty[Rule])
  }

  /**
   * The application of a rule. Wraps the rule up with a tracking index to count how many
   * symbols from the RHS have been successfully applied already.
   */
  case class RuleApplication(rule: Rule, subarcs: Seq[Arc] = Seq.empty[Arc], applied: Int = 0) extends HasRHS {
    val active: Boolean = rule.rhs.size > applied

    val (seen, remaining) = rule.rhs.splitAt(applied)

    override val nextSymbol: Option[Sym] = remaining.headOption

    def extendWith(arc: Arc): Option[RuleApplication] = if (nextSymbolIs(arc.symbol)) {
      Some(RuleApplication(rule, subarcs :+ arc, applied + 1))
    } else None

    override def toString = s"""${rule.lhs} --> ${seen.mkString(" ")} • ${remaining.mkString(" ")}"""
  }

  /**
   * Arcs are entries in the chart. Each arc:
   * - may be active (incomplete) or passive (complete);
   * - has a start and end index, describing the span of covered words;
   * - has a symbol, descibing the left-hand-side of the applied rule
   * - may be extended by applying the fundamental rule (if certain conditions are met)
   */
  sealed trait Arc {
    def start: Int
    def end: Int
    def symbol: Sym
    def active: Boolean
    def applyFundamental(arc: Arc): Option[Arc]
  }

  /**
   * WordArcs represent lexical entries added to the Chart. They hold no other structure, nor
   * do they necessarily relate to grammar.
   */
  case class WordArc(
      override val start: Int,
      override val end: Int,
      override val symbol: Sym) extends Arc {
    /** WordArcs are always passive, as they cover one word. **/
    override val active: Boolean = false
    /** WordArcs are always passive, so cannot be extended through fundamental rule **/
    def applyFundamental(arc: Arc): Option[Arc] = None
  }

  case class RuleArc(
      override val start: Int,
      override val end: Int,
      rule: RuleApplication) extends Arc with HasRHS {
    override val symbol = rule.rule.lhs
    def nextSymbol = rule.nextSymbol

    override val active = rule.active

    /**
     * The fundamental rule may be applied from this arc to another if:
     * [1] This arc ends where the proposed arc begins;
     * [2] This arc is active;
     * [3] This arc has the symbol of the proposed arc, next.
     */
    def applyFundamental(arc: Arc): Option[Arc] = if (arc.start == end) {
      rule.extendWith(arc).map(RuleArc(start, arc.end, _))
    } else None
  }

  case class Chart(arcs: Set[Arc], sentence: Seq[String]) {
    def nextChart(arc: Arc): (Chart, Set[Arc]) = {
      if (arcs(arc)) {
        (this, Set.empty[Arc])
      } else {
        val fundamentals: Set[Arc] =
          if (arc.active) {
            // new arc active; attempt to extend with passive rules on chart
            arcs.withFilter(a => !a.active).flatMap(arc.applyFundamental)
          } else {
            // new arc passive; attempt to extend any active rules from chart with new arc
            arcs.withFilter(_.active).flatMap(_.applyFundamental(arc))
          }
        // return chart with new arc added; also, new arcs to be added to next agenda
        (Chart(arcs + arc, sentence), fundamentals)
      }
    }
  }

  case class ParserState(agenda: Seq[Arc], chart: Chart)

  /**
   * Basic Chart parser, leaving abstract the methods for defining agenda extension.
   */
  trait ChartParser {

    def parse(grammar: Grammar)(sentence: Seq[String]): Chart = {
      extend(initialise(grammar)(sentence), grammar)
    }

    @tailrec
    private final def extend(state: ParserState, grammar: Grammar): Chart = state match {
      case ParserState(Seq(), chart) => chart
      case ParserState(Seq(arc, arcs @ _*), chart) =>
        // arcs from extending existing graph nodes
        val (newChart, extendedArcs) = chart.nextChart(arc)
        // arcs from new rules
        val newArcs = generateNewArcs(grammar)(arc)
        // generate the next agenda
        val newAgenda = nextAgenda(arcs, (extendedArcs ++ newArcs).toSeq)
        // recurse
        extend(ParserState(newAgenda, newChart), grammar)
    }

    /** Implement these to define the policy - bottom-up or top-down **/
    def initialise(grammar: Grammar)(sentence: Seq[String]): ParserState

    def generateNewArcs(grammar: Grammar)(arc: Arc): Set[Arc]

    /** Implement this to define the agenda-handling strategy (e.g. breadth- or depth-first) **/
    def nextAgenda(old: Seq[Arc], additions: Seq[Arc]): Seq[Arc]
  }

  object ChartParser {
    /** Generate an S-expression readout of the phrase-structure **/
    def readout(arc: Arc): String = arc match {
      case ra: RuleArc =>
        s"""(${ra.symbol} ${ra.rule.subarcs.map(readout).mkString(" ")})"""
      case a => a.symbol.toString
    }

    /** Select all active nodes, filtered by symbol **/
    def passiveNodes(chart: Chart, symbol: NonTerm): Set[Arc] = {
      chart.arcs.filter { arc => arc.symbol == symbol && !arc.active }
    }
  }

  /**
   * Starts with words and proceeds to find covering rules until it reaches the TOP symbol.
   */
  trait BottomUpChartParser extends ChartParser {
    /** Initialise the agenda with one WordArc for each word in the sentence **/
    override def initialise(grammar: Grammar)(sentence: Seq[String]): ParserState = {
      val agenda = sentence.zipWithIndex.map {
        case (word, index) => WordArc(index, index + 1, word)
      }
      ParserState(agenda, Chart(Set.empty[Arc], sentence))
    }

    /**
     * New arcs are generated bottom-up:
     * grammar rules are selected if their first RHS symbol is that required by the Arc.
     */
    override def generateNewArcs(grammar: Grammar)(arc: Arc): Set[Arc] = if (!arc.active) {
      grammar.rulesWhoseRhsStartsWith(arc.symbol).flatMap { rule =>
        RuleArc(arc.start, arc.start, RuleApplication(rule)).applyFundamental(arc)
      }
    } else { Set.empty[Arc] }
  }

  /**
   * Starts with the TOP symbol, and proceeds to expand down to the words
   */
  trait TopDownChartParser extends ChartParser {
    /** Initialise the agenda with one RuleArc for the top symbol **/
    override def initialise(grammar: Grammar)(sentence: Seq[String]): ParserState = {
      // all words to be added to the chart
      val chartWordArcs: Set[Arc] = sentence.zipWithIndex.map {
        case (word, index) => WordArc(index, index + 1, word)
      }(collection.breakOut)
      // top-down starting rule added to agenda
      val agenda = Seq(RuleArc(0, 0, RuleApplication(grammar.top)))
      ParserState(agenda, Chart(chartWordArcs, sentence))
    }

    /**
     * New arcs are generated top-down:
     * grammar rules are selected if their LHS symbol is the next symbol for the Arc.
     */
    override def generateNewArcs(grammar: Grammar)(arc: Arc): Set[Arc] = arc match {
      // only active arcs can be extended into new Arcs
      case RuleArc(start, end, rule) if rule.active =>
        grammar.rulesThatExpand(rule.nextSymbol.get).map { rule =>
          RuleArc(end, end, RuleApplication(rule))
        }
      case _ => Set.empty[Arc]
    }
  }

  /** Breadth-first - it's a queue **/
  trait BreadthFirstChartParser extends ChartParser {
    override def nextAgenda(old: Seq[Arc], newArcs: Seq[Arc]) = old ++ newArcs
  }

  /** Depth-first - it's a stack **/
  trait DepthFirstChartParser extends ChartParser {
    override def nextAgenda(old: Seq[Arc], newArcs: Seq[Arc]) = newArcs ++ old
  }

  trait BFBUChartParser extends BottomUpChartParser with BreadthFirstChartParser
  trait DFTDChartParser extends TopDownChartParser with DepthFirstChartParser
}