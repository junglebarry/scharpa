scharpa [![Build Status](https://travis-ci.org/junglebarry/scharpa.svg?branch=master)](https://travis-ci.org/junglebarry/scharpa)
=================

Scharpa is a Scala implementation of *active chart parsing* with context-free grammars.

The implementation is loosely based on Peter Hancox's courses on Natural Language Processing in Prolog, but the closest reference is [this paper][simpkins-hancox_90_chart-parsing]. This covers generalisation to allow different agenda processing policies and directional strategies. Our implementation also makes some recourse to [this course][blackburn-striegnitz2002nlp-prolog]. 

Scharpa is not intended as a wide-coverage, robust, or high-performance parser; rather, it is an implementation of an efficient and flexible algorithm for dealing with formal languages and limited-domain natural language. In particular, the bottom-up features of chart parsing are useful where complete parses cannot be recovered.

## Parsing

Parsing is the process of deriving a phrase-structure analysis of some input text according to a given grammar. This project considers only context-free grammars, with rules in [Backus-Naur Form]( http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form):

```Prolog
LHS -> RHS1, RHS2, ..., RHSn
```

Here, there is a single symbol on the left-hand side (LHS) that can be expanded into a (non-empty) sequence of symbols on the right-hand side (RHS). Each `RHSi` from the right-hand side can act as the left-hand-side of another rule in the grammar.

A prototypical example of such a grammar is:

```Prolog
S -> NP VP
NP -> DET N
DET -> "the"
N -> "cat"
VP -> V
V -> "sat"
```

and parsing the sentence "the cat sat" would yield the following phrase-structure analysis (warning: S-expressions follow):

```Scheme
(S (NP (DET "the") (N "cat")) (VP (V "sat")))
```

Parsing can be viewed as a search through the space of possible analyses for the one (or many) that best explain the observed sentence. Of course, it's entirely possible that the given grammar doesn't describe what might intuitively appear to be a perfectly valid sentence.

The *parsing as search* viewpoint is important for what follows. There are a few reasons of particular relevance here.

First, context-free grammars allow recursive rules of the form `A -> A B`, which could cause a naïve top-down search to enter infinite [left recursion](http://en.wikipedia.org/wiki/Left_recursion).

Second, a common mechanism for searching through a space of possibilities is to use backtracking to consider alternative expansions, which can lead to wasteful recomputation.

Third, depending on the domain, the precise formal analysis of a sentence may not be possible to derive, but partial structures would be a useful fallback position. Many standard parsing algorithms don't make it easy to retrieve these partial analyses.

Finally, some common techniques for controlling standard state-space search are applicable to the parsing problem, such as changing *how* we explore new nodes.

In our system, the grammar is represented as a `Grammar` type, which comprises: 

* a `Set[Rule]`, where each `Rule` is a rule in the grammar (form described above)
* a special *top rule*, which provides the root symbol of any syntactic analysis, stored as a `TopRule`.

## Chart parsing

Chart parsing is a [dynamic programming](http://en.wikipedia.org/wiki/Dynamic_programming) technique for building a parse structure according to a given grammar. It attempts to build a *chart* for an input sentence, where the chart is a set of partially-applied rules from the grammar, called *arcs*. Arcs are defined as having:

* a starting index within the sentence
* an ending index within the sentence
* (thus) a contiguous subsequence of "covered" words
* a rule, which is being applied.

The chart grows over time through a process of *extension*: existing arcs are tested to see if they can cover some more of the sentence, and if so, a new arc is added to the chart, covering this extra information. Importantly, arcs are never removed from the graph; they are extended to produce new arcs. This is the basis of dynamic programming: the knowledge we have gleaned so far is retained and never thrown away.

Arcs fall into one of two states:

* **Active**: the arc's rule can cover more symbols, so can still be extended;
* **Passive**: all of the arc's rule can cover more symbols, so can still be extended;

These states dictate how a particular arc may be used to extend the chart.

Our system defines types for `Chart` and `Arc`. It goes further to distinguish subtypes of `Arc` for words (`WordArc`) and partially-applied rules (`RuleArc`).  The algorithm for parsing is encapsulated by the `ChartParser` type.

## Agendas

Chart parsers can be customised to search through hypothesised analyses in a chosen order. Standard state-space search uses an *agenda* to control the order in which hypotheses are examined. The agenda is an ordered sequence of `Arc`s, where the first will be examined next. New `Arc`s are added to the agenda, and the resulting sequence determines the overall search policy.

The `ChartParser` is agnostic to search order, so leaves a abstract method:

```scala
def nextAgenda(old: Seq[Arc], additions: Seq[Arc]): Seq[Arc]
```

Two naïve agenda implementations are provided: `DepthFirstChartParser` and `BreadthFirstChartParser`.

## Strategies

There are two major strategies for parsing: 

* **top-down** starts with a special rule in the grammar, called the *top node*, and attempts to expand it to meet the words of the sentence.
* **bottom-up** starts with the words of the sentence, and attempts to find rules that cover them until it reaches the *top-node*.

These strategies are relevant when initialising the `Chart` and `Agenda`, and when expanding an `Arc` from the agenda. The chart parsing algorithm is agnostic to the strategies, so our `ChartParser` type leaves unspecified two methods:

```scala
def initialise(grammar: Grammar)(sentence: Seq[String]): State

def generateNewArcs(grammar: Grammar)(arc: Arc): Set[Arc]
```
Two existing strategies are provided: `BottomUpChartParser` and `TopDownChartParser`.

## Examples

See [the tests](https://github.com/junglebarry/scharpa/blob/master/src/test/scala/junglebarry/scharpa/SimpleChartParserSpec.scala) for examples of how to construct a simple grammar and apply the parser.

```scala
  // construct a grammar
  val the = SimpleRule("Det", "the")
  val cat = SimpleRule("N", "cat")
  val dog = SimpleRule("N", "dog")
  val sat = SimpleRule("V", "sat")
  val np = SimpleRule("NP", "Det", "N")
  val vp = SimpleRule("VP", "V")
  val s = SimpleRule("S", "NP", "VP")
  val rules = Set[Rule](the, cat, dog, sat, np, vp, s)
  val top = TopRule("S")
  val grammar = new Grammar(rules, top)

  // build a parser - this one is bottom-up, breadth first
  val parser = new BottomUpChartParser with BreadthFirstChartParser {}

  // apply to a sentence
  val chart = parser.parse(grammar)(Seq("the", "cat", "sat"))

  // print the chart
  println("=== Complete chart ===")
  chart.chart.foreach(println)
  println("======================")
```

## References

* [Neil K. Simpkins and Peter Hancox (1990) Chart parsing in Prolog. New Generation Computing 8(2), pp. 113-138.][simpkins-hancox_90_chart-parsing]
* [Patrick Blackburn and Kristina Striegnitz (2002) Natural Language Processing Techniques in Prolog][blackburn-striegnitz2002nlp-prolog]

[simpkins-hancox_90_chart-parsing]: http://www.cs.bham.ac.uk/~pjh/publications/ngc_90.pdf  "Neil K. Simpkins and Peter Hancox (1990) Chart parsing in Prolog. New Generation Computing 8(2), pp. 113-138."
[blackburn-striegnitz2002nlp-prolog]: http://cs.union.edu/~striegnk/courses/nlp-with-prolog/html/index.html "Patrick Blackburn and Kristina Striegnitz (2002) Natural Language Processing Techniques in Prolog"