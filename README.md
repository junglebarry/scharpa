scharpa [![Build Status](https://travis-ci.org/junglebarry/scharpa.svg?branch=master)](https://travis-ci.org/junglebarry/scharpa)
=================

A simple Scala implementation of *active chart parsing* with context-free grammars.

The implementation is loosely based on [this course](http://cs.union.edu/~striegnk/courses/nlp-with-prolog/html/index.html), but is extended to allow different agendas and strategies.

## Parsing

Parsing is the process of deriving a phrase-structure analysis of some input text according to a given grammar. This project considers only context-free grammars, with rules of the form:

  LHS -> RHS1, RHS2, ..., RHSn

where a single symbol on the left-hand side (LHS) can be expanded into a (non-empty) sequence of symbols on the right-hand side (RHS). Each `RHSi` from the right-hand side can act as the left-hand-side of another rule in the grammar.

A prototypical example of such a grammar is:

  S -> NP VP
  NP -> DET N
  DET -> "the"
  N -> "cat"
  VP -> V
  V -> "sat"

## Chart parsing

Chart parsing is a dynamic programming technique for building a parse structure according to a given grammar. It attempts to build a *chart* for an input sentence, where the chart is a set of partially-applied rules from the grammar, called *arcs*. Arcs are defined as having:

* a starting index within the sentence
* an ending index within the sentence
* (thus) a contiguous subsequence of "covered" words
* a rule, which is being applied.

The chart grows over time through a process of *extension*: existing arcs are tested to see if they can cover some more of the sentence, and if so, a new arc is added to the chart, covering this extra information. Importantly, arcs are never removed from the graph; they are extended to produce new arcs. This is the basis of dynamic programming: the knowledge we have gleaned so far is retained and never thrown away.

Arcs fall into one of two states:

* **Active**: the arc's rule can cover more symbols, so can still be extended;
* **Passive**: all of the arc's rule can cover more symbols, so can still be extended;

Chart parsers can be customised to adopt particular strategies

It differs from simple parsing systems in a number of important ways:

0. The "chart" for a sentence is built and extended, but we never 

