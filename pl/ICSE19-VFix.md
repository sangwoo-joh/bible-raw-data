# VFix: Value-Flow-Guided Precise Program Repair for Null Pointer Deferences

## Introduction
 - APR: Automated Program Repair
 - two phases:
   1. fault localization: find `L` (a set of suspicious). may trigger
      a bug after executing at least one failing test case in a test
      suite.
   2. patch generation: find `O`, generate & validate using test suite
 - plausible patch: one that passes all the test cases in the test
   suite
 - correct patch: results in correct outputs for all possible program
   inputs

### Challenges
 - efficiently, correct, from a potentially infinite solution space(`L
   x O`).
 - resulting solution space = `L x O` = `C` U `I` U `X` (U is Union),
   where
   - `C`: a set of correct patches
   - `I`: a set of implausible patches
   - `X`: a set of plausible but incorrect patches
 - the ratio of the correct patches over the incorrect pathces(`C` /
   `I`U`X`) is extremely small.

### Prior Work
 - existing efforts typically accelerate the repair process
 - by ranking patches based on their probabilities of being correct,
   i.e., executing only the high-priority pathces.
 - `L'`: reduced `L`, usually by stress testing with a limited number
   of test cases
 - `O'`: reduced `O`, usually adopting a variety of heuristics,
   e.g. syntactic/semantic code search, statistical analysis, symbolic
   execution, machine learning.

#### Two categories of APR approaches
##### 1. General-Purpose
 - theoretically applied to all kinds of bugs

###### Search-based
 - adopt a generate-and-validate process then generated candidate
   patches by exhaustively exploring the solution space, using
   e.g. genetic proramming or random search, and then validates the
   patches with a test suite.
 - the large search space hinders their efficiency and scalability ->
   heuristics(narrowing down the scope) , repair templates(guiding the
   search to generate correct patches efficiently)
 - `CAPGEN`: leverages context information extracted from AST to
   achieve fine-grained patch prioritization.

###### Semantics-driven
 - view a repair task as a program synthesis problem and synthesize
   patches via constraint-solving.
 - All other APR tools in [Evaluation](#Evaluation)

##### 2. Bug-Specific
 - designed for specific types of bugs
 - restrict their scope to some types of bugs only.

### Limitation
 - data/control dependency matters
 - the per-bug time budget is often set up as 3-5 hours for heavy
   testing.
 - NPE: 37.2% of all memory bugs in Mozilla/Apache, over 40% of
   exceptions in Android.
 - `Defects4j`: a well-known benchmark suite often used for validating
   APR
   - 15 NPEs in `Defects4j`
   - existing tools correctly repair only at most 4 NPEs

#### Difficulty of repair NPE
 - NPE location and its bug-fixing location can span across multiple functions
 - a wide variety of programming mistakes
   - missing null pointer checks
   - missing object initialization
 - Thus, possibly infinite many repair operations at a large number of
   suspicious statements.

### Insights
 - **Static analysis is relatively unexplored for automated program
   repair**. This paper aims to make one step forward in investigating
   how to apply static value-flow analysis, which resolved both the
   data and control flow of a program, to help APR generate a precise
   solution space by increasing the number of correct pathces
   generated for repairing NPEs.

 - for obtaining `L'` (fault localization): control dependency should
   be considered, otherwise coarse-grained selection usually produces
   an imprecise `L'`.
 - for obtaining `O'` (patch generation): data/control dependencies
   should be considered, otherwise generate incorrect repair
   operations.
 - example of plausible but incorrect patch: fixes only bugs in that
   line, and breaks data flow afterwards.


### Solution
 - value-flow-guided APR approach
 - considering a substantially reduced solution space in order to
   increase the number of correct patches generated.
 - by incorporating with data and control dependence information

 1. construct an inter-procedural VFG (a static value-flow slice of
    the program)
 2. formulate fault localization problem by
     1. identifying the set of suspicious statments (selecting repair
        locations)
     2. rank them by solving a graph congestion calcuation problem on
        the static slice.
 3. produce a precise set of value-flow aware repair operations


### Contribution
 - APR precisely and efficiently fixing NPE bugs by considering a
   substantially reduced solution space.
 - formulate fault localization by (1) identifying the suspicious
   problems based on static value-flow analysis and dynamic execution
   and (2) rank them by solving a graph congestion calculation
   problem.
 - for `Defect4j` and existing 8 APR tools, VFIX outperform SIMFIX by
   generate 3x fix (12 / 4) and efficient in minutes instead of hours.


## Approach
### Constructing Static Value-Flow Slices
 - Input: a buggy program, one NPE crash site
 - Output: inter-procedural value-flow graph (VFG) `G_{sta} =
   (L_{sta}, E_{sta})`
   - a directed graph that captures all the potential NPE-triggering
     sources and other related NPE crash sites
   - `L_{sta}`: the set of nodes representing statements
   - `E_{sta}`: the set of edges representing their def-use relations
 - The target language is a subset of Java language.
 - Let `p.use()` be an NPE crash site, where `p` is null. We obtain
   its static value-flow slice by solving `G_{sta}(i+1) =
   f_{vfg}(G_{sta}(i))` iteratively, starting `G_{sta}(0) = {p}`,
   until a fixed pointer is reached, where `f_{vfg}` consists of
   applying the rules.

### Selecting and Ranking Repair Locations
 - `L_{dyn}`: the set of statements executed by the **NPE-triggering
   test case**.
 - Only consider the repair statements in the dynamic slice `L_{vf} =
   L_{dyn} /\ L_{sta}`
 - Rank the repair statements by solving a **congestion calculuation
   problem** on `G_{sta}`
   - The intuition behind is that a repair location with *a higher
     congestion value* has a better chance to avoid also the other
     related potential NPE bugs that are not discovered by the
     NPE-triggering test case, thus recuding more effectively the
     number of plausible but incorrect patches generated.

### Applying Value-Flow-Aware Repair Operations
#### 1) NPE Bug Model
 - Two scenarios of NPE in real-world programs:
   1. object is not initialized on some path leading to use-site
   2. a "null check" for the use-site is missing
 - Assuming two:
   - `call-graph integrity`: API calls are invoked correctly.
   - `type integrity`: variables are type correctly in their
     declarations.

#### 2) Repair Operations
 - Two repair templates:
   1. `initialization`: aims to initialize a `null` pointer by
      assigning a newly created object
   2. `skip`: aims to avoid executing an NPE-triggering statements and
      other related ones (guard)

 .... and some more details ....


## Evaluation
 - to show that VFix can significantly outperform the state-of-the-art
   for repairing NPE bugs in terms of both precision and efficiency
 - 8 representative APR tools
   - General-purpose: `JGENPROG`, `JKALI`, `NOPOL`, `ACS`, `CAPGEN`,
     `HDREPAIR`, `SIMFIX`
   - Specialized for NPEs: `NPEFIX`
 - 15 NPE bugs in `Defects4j` version 1.0.1
 - Intel Core i5 3.20 GHz CPU, 4GB memory, Ubuntu 16.04, jdk 1.6.0_45,
   maximum heap size of JVM as 4GB.
 - *correctness* of patch: passes all the test cases in the tes
   suite + semantically/syntactically equivalent to a human-written
   patch

| Project | LoC | # of NPEs | Time Spent(sec) |
| --- | ---:| ---:| --- |
| `JFreeChart` | 96 KLoc | 7 | `OOB`, 93, 87, 107, 31, 91, 95 |
| Apache `commons-lang` | 22 KLoc | 5 | 30, 36, 33, 32, 27 |
| Apache `commons-match` | 85 KLoc | 3 | 34, 30, `OOB` |
| `Joda-Time` (Calendar system) | 28 KLoc | 0 | - |

 - VFix achieved an average reduction of 94.11 % by moving away from
   the space of suspicious statements selected by a general-purpose
   fault localization tool (`GZoltar`) to `L_{vf}`.
 - By leveraing the value-flow information for an NPE-triggering site,
   VFix avoids many irrelevant repair locations that would otherwise
   be selected by such bug-type-unaware spectrum-based fault
   localization technique.
 - VFix also has significantly reduced the number of repair operations
   in `O_{vf}` by comparing the number of repair templates
   instantiated with and without our value-flow analysis at a
   statement where a correct fix is made. (a reduction of 57 % on
   average) -> avoiding unnecessary repair operations tried and
   boosting the efficiency of patch generation.
