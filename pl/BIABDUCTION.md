# Go Huge or Go Home

 [Reference](https://blog.sigplan.org/2020/03/03/go-huge-or-go-home-popl19-most-influential-paper-retrospective/)

 Bi-abduction allows an analysis to determine separation logic
 pre-conditions under which the procedure executes safely, and
 corresponding post-conditions that describe how the procedure mutates
 the heap when executed from a configuration satisfying the
 precondition.


> What is the main problem blocking application to one million lines
> of code?  The need for the human to supply preconditions, we need a
> **compositional** version of our techniques that discovers
> preconditions.


 The notion of compositionaly comes originally from language
 semantics: a semantic is compositional if the meaning of a compound
 phrase is defined in terms of the meanings of its pars and a method
 of combining them. Typically, *denotational semantic* is
 compositional where *operational semantic* is **not**.

 An analysis is compositional if the analysis result of a compound
 program is defined in terms of the analysis results of its parts and
 a means to combine them.


 Three benefits of compositional analyses, compared to global ones:
  1. *The ability to analyze program parts, without having the
     context.*
  2. *Potential to scale.*
  3. **Graceful imprecision.** If an analysis gets confused on one
     area of code, the resulting imprecision need not leak into
     analyses of code that follows it.


 Compositional algorithms can also have a drawback compared to their
 global cousins: they can lose *precision*.

 Usually, logic works with vhalidity or entailment statements like:

```
A ㅏ B
```

 which says that A implies B. Bi-abduction is an extension of this
 question of the form

```
A * ?antiframe ㅏ B * ?frame
```

 Here, A and B are formulae that are given, and the problem is to
 *discover* a pair of *frame* and *antiframe* formulae that make the
 entailment statement valid. The `*` in this question is the
 *separating conjunction* of separation logic. The antiframe is an
 *abductive hypothesis*: it represents missing resource which is in B
 but not A. In contrast, the frame refers to additional resource, in B
 but not A.

 + A: "point x is owned" `*` "point y is owned"
 + B: "point y is owned" `*` "point z is owned"

```
  "x is owned"        ?frame
* "y is owned"   ㅏ  * "y is owned"
* ?antiframe         * "z is owned"
```

 + antiframe: "pointer z is owned"
 + frame: "point z is owned"


 Abduction was advanced by the philosopher Charles Peirce in his work
 on explicating the scientific method, where he distinguished
 *hypothesis discover* (**abduction**) from **deduction** (*comfirming
 true conclusions*) and **indcution** (*generalization*) inference
 patterns. Abduction intuitively fits the problem of precondition
 discovery very well: the inferred preconditions are the
 hypotheses. What is more, abduction often seeks the "best" or at
 least a "small" solution, and this meshes with the idea to attempt to
 infer a description of the footprint rather than a description of the
 global state.


 .... Suppose algorithm A (which incidentally does not often itself
 have independent justification as regards precision) never gives you
 results because it's too slow, and algorithm B terminates and gives
 you answers. You shouldn't then complain about algorithm B because it
 doesn't match A. Rather, you should ask *why algorithm A should exist
 in the first place*, because it has much less potential **to help
 people.**

 The demo mentality helpled Infer to succeed when we took it inside
 Facebook. Rather than show people a table of (say) runtimes and
 (putative) false positive rates, we would **run** the tool and show
 them specific reports.

 While demos are cool, it's still very important to measure as well as
 to show, and there we ended up using a concept - fix rate, the
 proportion of reports fixed by engineers - that is easier to measure
 thatn the precision or false positive rate. *Fix rate* has an
 advantage over raw analyzer precision in that it speaks to the "does
 it help people?" aspect of analysis quality; it's based on how people
 react to analyzer reports.

 Another key learning was that the value of analysis results **decays
 over time** to deliver. In one example, a fast, diff-time deployment
 saw a 70% fix rate where the same analysis run globally, overnight,
 saw a 0% fix rate. Analyzing code modifications enables timely
 feedback that fits with developers' workflows. These observations led
 us to make further analyses, not always based on bi-abduction, but
 retaining the begin-anywhere capability.
