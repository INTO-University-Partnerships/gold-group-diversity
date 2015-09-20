# GOLD group diversity

A command line programme to maximize the diversity (in terms of gender, INTO centre and country of origin) of groups of students within a course.

## Technology

* [Stack](https://github.com/commercialhaskell/stack) is required to build the programme
* It is currently built against [LTS Haskell 3.5](https://www.stackage.org/lts-3.5)
* [In-depth guide](https://github.com/commercialhaskell/stack/blob/master/GUIDE.md) to Stack

## Theory

### Algorithm

The algorithm used to solve the so-called *"maximally diverse grouping problem"* (MDGP) is the *"LCW"* algorithm presented in [this paper](http://www.uv.es/rmarti/paper/docs/mdp7.pdf) on page 5. In pseudo-code, this reads:

```
do {
    for (i = 1, ..., M) {
        1. Find the element j in any group for which a switch of group assignments between
           elements i and j results in the largest increase in the objective function value.
        2. If the increase in the objective function is strictly positive, make the switch.
    }
} while at least one switch is made.
```

The *do-while* block is implemented by the functions [diversifyCourse](https://github.com/INTO-University-Partnerships/gold-group-diversity/blob/master/src/Lib.hs#L30) and [anySwitches](https://github.com/INTO-University-Partnerships/gold-group-diversity/blob/master/src/Lib.hs#L61). Steps *1.* and *2.* are together implemented by the function [switchUserPair](https://github.com/INTO-University-Partnerships/gold-group-diversity/blob/master/src/Lib.hs#L75).

### Quantifying diversity

Diversity is quantified by the so-called *"objective function"* explained in the paper on pages 2 and 3. However, calculating the objective function for the *entire* set of groups *G* is not necessary. Rather, it is only necessary to consider deltas in the objective function value between two groups when evaluating whether to make a switch. Calculating objective function deltas is implemented by the function [objectiveFunctionDelta](https://github.com/INTO-University-Partnerships/gold-group-diversity/blob/master/src/Lib.hs#L118).

The *"objective function"*  is, appropriately enough, implemented by the function [objectiveFunction](https://github.com/INTO-University-Partnerships/gold-group-diversity/blob/master/src/Lib.hs#L49) (although the implementation is for one particular group, not all *G* groups). The implementation of the *"objective function"* does not take the square root of the sum of the differences between each pair of users. Instead, it simply sums the number of attributes that differ between pairs. As there are three attributes (gender, INTO centre and country of origin) the possible difference values between a pair of users is `[0, 1, 2, 3]`.

## Build

    stack build --pedantic

## Test

    stack test --pedantic

## Example

    $ more data/homogeneous.csv | build/diversify --size 8
    "30","F","CIT","FR","Group A"
    "07","F","NCL","GB","Group A"
    "24","F","CIT","GB","Group A"
    "14","F","NCL","FR","Group A"
    "42","M","NCL","FR","Group A"
    "50","M","CIT","GB","Group A"
    "49","M","CIT","GB","Group A"
    "41","M","NCL","FR","Group A"
    "12","F","NCL","FR","Group B"
    "25","F","CIT","FR","Group B"
    "23","F","CIT","GB","Group B"
    "06","F","NCL","GB","Group B"
    "59","M","CIT","FR","Group B"
    "58","M","CIT","FR","Group B"
    "34","M","NCL","GB","Group B"
    "33","M","NCL","GB","Group B"
    "29","F","CIT","FR","Group C"
    "21","F","CIT","GB","Group C"
    "51","M","CIT","GB","Group C"
    "60","M","CIT","FR","Group C"
    "36","M","NCL","GB","Group C"
    "35","M","NCL","GB","Group C"
    "13","F","NCL","FR","Group C"
    "10","F","NCL","FR","Group C"
    "57","M","CIT","FR","Group D"
    "44","M","NCL","FR","Group D"
    "52","M","CIT","GB","Group D"
    "37","M","NCL","GB","Group D"
    "08","F","NCL","GB","Group D"
    "03","F","NCL","GB","Group D"
    "31","F","CIT","FR","Group D"
    "32","F","CIT","FR","Group D"
    "43","M","NCL","FR","Group E"
    "53","M","CIT","GB","Group E"
    "27","F","CIT","FR","Group E"
    "26","F","CIT","FR","Group E"
    "19","F","CIT","GB","Group E"
    "11","F","NCL","FR","Group E"
    "39","M","NCL","GB","Group E"
    "40","M","NCL","GB","Group E"
    "61","M","CIT","FR","Group F"
    "20","F","CIT","GB","Group F"
    "18","F","CIT","GB","Group F"
    "17","F","CIT","GB","Group F"
    "04","F","NCL","GB","Group F"
    "46","M","NCL","FR","Group F"
    "47","M","NCL","FR","Group F"
    "48","M","NCL","FR","Group F"
    "62","M","CIT","FR","Group G"
    "38","M","NCL","GB","Group G"
    "28","F","CIT","FR","Group G"
    "16","F","NCL","FR","Group G"
    "09","F","NCL","FR","Group G"
    "05","F","NCL","GB","Group G"
    "55","M","CIT","GB","Group G"
    "56","M","CIT","GB","Group G"
    "54","M","CIT","GB","Group H"
    "45","M","NCL","FR","Group H"
    "22","F","CIT","GB","Group H"
    "15","F","NCL","FR","Group H"
    "02","F","NCL","GB","Group H"
    "01","F","NCL","GB","Group H"
    "63","M","CIT","FR","Group H"
    "64","M","CIT","FR","Group H"
    "65","F","XXX","YY","Group I"
