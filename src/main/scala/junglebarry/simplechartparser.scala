package junglebarry

import scala.annotation.tailrec

package object simplechartparser {

  trait HasRHS {
    def nextSymbol: Option[String]

    def nextSymbolIs(symbol: String): Boolean = { nextSymbol.exists(_ == symbol) }
  }

  /**
   * Rules are context-free grammar rules, which have a single left-hand-side entry,
   * and expand to one or more right-hand-side entries (enforced by type).
   */
  case class Rule(lhs: String, rhsHead: String, rhsTail: String*) extends HasRHS {
    lazy val rhs: Seq[String] = rhsHead +: rhsTail.toSeq
    override lazy val nextSymbol: Option[String] = rhs.headOption
  }

  /** A grammar is a set of rules **/
  case class Grammar(rules: Set[Rule]) {
    lazy val rulesLookup: Map[String, Set[Rule]] = rules.filter(_.nextSymbol.isDefined).groupBy(_.nextSymbol.get)
  }

  case class RuleApplication(rule: Rule, applied: Int = 0) extends HasRHS {
    lazy val active: Boolean = rule.rhs.size > applied

    lazy val (seen, remaining) = rule.rhs.splitAt(applied)

    override lazy val nextSymbol: Option[String] = remaining.headOption

    override def toString = s"""${rule.lhs} --> ${seen.mkString(" ")} • ${remaining.mkString(" ")}"""
  }

  /**
   * Arcs are entries in the chart. Each arc:
   * - may be active (incomplete) or passive (complete);
   * - has a start and end index, describing the span of covered words;
   * - has a symbol, descibing the left-hand-side of the applied rule
   * - may be extended by applying the fundamental rule (if certain conditions are met)
   */
  trait Arc {
    def start: Int
    def end: Int
    def symbol: String
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
      override val symbol: String) extends Arc {
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
    def applyFundamental(arc: Arc): Option[Arc] = {
      if (arc.start == end && rule.nextSymbolIs(arc.symbol)) {
        Some(RuleArc(start, arc.end, rule.copy(applied = rule.applied + 1)))
      } else None
    }
  }

  case class Chart(arcs: Set[Arc]) {
    def extend(arc: Arc): (Chart, Seq[Arc]) = {
      val fundamentals =
        if (arc.active) {
          // new arc active; attempt to extend with passive rules on chart
          arcs.filterNot(_.active).flatMap(arc.applyFundamental(_))
        } else {
          // new arc passive; attempt to extend any active rules from chart with new arc
          arcs.filter(_.active).flatMap(_.applyFundamental(arc))
        }
      // return chart with new arc added; also, new arcs to be added to next agenda
      (Chart(arcs + arc), fundamentals.toSeq)
    }
  }

  case class State(agenda: Seq[Arc], chart: Chart)

  trait ChartParser {

    def initialise(grammar: Grammar)(sentence: Seq[String]): State

    def parse(grammar: Grammar)(sentence: Seq[String]): Chart = {
      extend(initialise(grammar)(sentence), grammar)
    }

    def nextAgenda(old: Seq[Arc], additions: Seq[Arc]): Seq[Arc]

    def generateNewArcs(grammar: Grammar)(arc: Arc): Set[Arc]

    @tailrec
    private final def extend(state: State, grammar: Grammar): Chart = state match {
      case State(Seq(), chart) => chart
      case State(arc :: arcs, chart) =>
        // arcs from extending existing graph nodes
        val (newChart, extendedArcs) = chart.extend(arc)
        // arcs from new rules
        val newArcs = generateNewArcs(grammar)(arc)
        // generate the next agenda
        val newAgenda = nextAgenda(arcs, extendedArcs ++ newArcs.toSet)
        // recurse
        extend(State(newAgenda, newChart), grammar)
    }
  }

  trait BottomUpChartParser extends ChartParser {
    /** Initialise the agenda with one WordArc for each word in the sentence **/
    def initialise(grammar: Grammar)(sentence: Seq[String]): State = {
      val agenda = sentence.zipWithIndex.map {
        case (word, index) => WordArc(index, index + 1, word)
      }
      State(agenda, Chart(Set.empty[Arc]))
    }

    /**
     * New arcs are generated bottom-up:
     * grammar rules are selected if their first RHS symbol is that required by the Arc.
     */
    def generateNewArcs(grammar: Grammar)(arc: Arc): Set[Arc] = {
      grammar.rulesLookup.get(arc.symbol).getOrElse(Set.empty[Rule]).flatMap { rule =>
        RuleArc(arc.start, arc.start, RuleApplication(rule)).applyFundamental(arc)
      }
    }
  }

  /** Breadth-first - it's a queue **/
  trait BreadthFirstChartParser extends ChartParser {
    def nextAgenda(old: Seq[Arc], newArcs: Seq[Arc]) = old ++ newArcs
  }

  trait BFBUChartParser extends BottomUpChartParser with BreadthFirstChartParser

}