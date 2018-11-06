# Tidal by example

## Transforming sequences

There are two parts to Tidal, a 'little language' for describing
sequences, and a library of functions for transforming them as
patterns. The little language for sequences is denoted with double
quotes:

```{.haskell render="colour"}
"red pink"
```

The above and following figures should be read clockwise, beginning at the top. The above pattern is rendered above in a circle, in order to put Tidal's central notion of cyclic pattern in visual form --- the end of one cycle is also the beginning of the next. There is much that we can do inside these double quotes to describe complex sequences, but first, lets introduce a simple way to transform this sequence using the `fast` function to 'speed it up':

```{.haskell render="colour"}
fast 5 "red pink"
```

Now we can see five repetitions of red and pink within a single cycle. It is worth noting that the number `5`, as a parameter to `fast` in the above, is itself a pattern. When we give a pattern as a 'bare' number like this, it simply repeats that number, once per cycle. The below gives a sequence of two numbers to `fast` instead, so that the first half of each cycle is 'speeded up' by a factor of five, and the second half by a factor of three. 

```{.haskell render="colour"}
fast "5 3" "red pink"
```

In the above, the `fast` function takes two patterns as input, and combines them to return a new pattern. Pattern transformations tend to operate relative to cycles, but that does not mean that successive cycles are identical. For example in the following, `every 3` is used to apply the function `fast "5 3"` to `"red pink"` as above, but only every third cycle. The first six cycles of the resulting pattern are shown below, so that you can see this change over time.

```{.haskell render="colour" cycles=6}
every 3 (fast "5 3") "red pink"
```

We can also squeeze the above six cycles into one, again by 'speeding it up', so that each cycle is identical once more:

```{.haskell render="colour" cycles=6}
fast 6 (every 3 (fast "5 3") "red pink")
```

Already we can see a strong part of Tidal's flexibility; it is highly *composable*. Functions like `fast 6` take a pattern as input, and return a new pattern as output, so that it is straightforward to compose multiple functions together into more complex transformations, as we have done above. Furthermore, we did not have to write any code to align `"5 3"` with `"red pink"`; in conventional terms, Tidal is *declarative*, in the sense that it takes care of the mechanics of pattern composition for us.

## An aside: Imperative, declarative and kairotic programming

Different techniques in programming are generally classified into either imperative or declarative programming. Imperative programming is where we describe a procedure in terms of how it is done. The usual everyday example of such a program is a cake recipe. Instead of describing a cake, a recipe gives a step-by-step procedure for combining different elements, where a cake is the (hopeful) result. In declarative programming, we instead describe *what* we want, and leave the job of *how* this is achieved to the computer. From the point of view of pattern-making, this distinction is already problematic in itself, in that *how* we make something is part of what it *is* [@mclean_artist-programmers_2011, pp 75-77].

Here we suggest that *Kairotic coding* is a third approach which sits between both declarative and imperative programming. This is akin to answering neither *how* or *what* questions, but instead focussing on programmers asking a question themselves: *what if*?. So they make changes to the structure of code, perceive the end result, and then make their next change to the code in response. This kairotic approach, where the programmers enter the timeline of their code, has become embedded in the practice-led design of TidalCycles. 

## Event time in polyphonic and Polymetric sequencing

We have already learned the standard unit of time in Tidal is the cycle. One impact of this is that if you add additional steps to a sequence, the steps will become shorter in duration, so that they are contained exactly within a single cycle:

```{.haskell render="multicolour"}
"red pink orange"
"red pink orange blue lightblue"
```

In the musical domain, this means that the more events you add to a pattern, the faster they will be played, in order to fit them into a cycle:

```{.haskell render="audio" fn="a.wav"}
sound "bd cp sd"
```

  ```{.haskell render="audio" fn="b.wav"}
sound "bd cp sd mt lt"
```

In other words, timing in Tidal is not based around a notion of a fixed 'beat' duration, but on higher level cycles. This lends metrical flexibility, so for example it is possible to break down individual steps into subcycles, using square brackets:

```{.haskell render="colour"}
"red [orange green] blue [lightblue yellow black]"
```

It is also possible to place more than one subcycle inside a single step:

```{.haskell render="colour"}
"[orange purple, lightblue yellow black]"
```
In the above we can see that `lightblue yellow black` fills the same span as `orange purple`. It is not significant that one subcycle is placed on the outside of the other; in musical terms this simply indicates that they form a polyphony, happening at the same time. We can change this behaviour by instead using curly brackets to denote the subsequences:

```{.haskell render="colour" cycles=6}
"{orange purple, lightblue yellow black}"
```

We can see that the *steps* rather than cycles now align, so that `orange purple` lines up with `lightblue yellow` in the first cycle. We can also see what it means to be a 'subcycle', as the subcycle continues where it left off, over successive cycles. From `lightblue yellow black`, only the first two colours are used in the first cycle to match with the two colours in `orange purple`, so on the second cycle it continues with `black`, cycling back to `lightblue` for its second value. We end up with a structure that repeats every third cycle.

Lets listen to the equivalent in the sound domain, listening to a two-step bass drum (`bd`) - clap (`cp`) sequence against a low (`lt`) - mid (`mt`) - high tom (`ht`) sequence:

```{.haskell render="audio" fn="c.wav"}
sound "[bd cp, lt mt ht]"
```

```{.haskell render="audio" fn="d.wav"}
sound "{bd cp, lt mt ht}"
```

Subcycles can be placed within subcycles. The following contains a subcycle with three steps (with a span of one third of a cycle each), of which the middle step is broken down further into two substeps (one sixth of a cycle each):

```{.haskell render="colour"}
"red [orange [black green] brown] blue"
```

## Rests, gaps and stretches

Silence is of course central to music, and in Tidal sequences you can
insert empty gaps with the `~` character.

```{.haskell render="colour"}
"red blue ~ orange [purple ~] green"
```

Alternatively a `_` character will stretch the previous step:

```{.haskell render="colour"}
"red blue _ orange [purple ~] green"
```

You can also use a `?` character to replace a step with silence around 50% of the time, varying from one cycle to the next:
```{.haskell render="colour" cycles="6"}
fast 12 "red blue?"
```

## Manipulating time

Music is of course a time-based artform, and in Tidal, time is malleable --- it both flows in cycles, and develops over time. It can be reversed, shifted forward into the future or back into the past, expanded and contracted, chopped up and rearranged, and subdivided to practically any depth.

We have so far focussed on sequences, but there is a much more to pattern. Let us move on to explore different kinds of patterning, all of which take one or more existing patterns and transform them. The nature of the pattern transformation might be perceivable by the listener, or perhaps only give them a sense of order amongst chaos, but because there is a clear structure in the creation of pattern, the sonic environment that results has the possibility to be an engaging place, explored through the process of listening. Of course, everyone listens differently, and so a pattern is not necessarily a puzzle to be solved, but an environment to be explored.

Many computer music systems represent music as *lists* of events, an approach which certainly has its advantages. However, Tidal instead represents music as a pure, mathematical *function*, with a timespan as input, and returning events active within that timespan as output. Each event consists of a value, and a timespan during which the event is active. This approach, a form of Functional Reactive Programming [@hudak_haskell_2000], allows the temporal structure of Tidal patterns to be efficiently manipulated without being calculated, either as a discrete or continuous signal, and separately from the events which are represented within the signal [@mclean_making_2014].  Time itself is represented as a rational number, lending itself to precise subdivision. It is not important to understand Tidal's inner representation of time in detail, but worth noting that much of the flexibility seen in the following stems from Tidal's focus on composing together functions of time, rather than linear procedures over data.

We have already seen the `fast` function for 'speeding up' a pattern. The `<~` and `~>` operators manipulate time a different way, by moving patterns backwards and forwards in time. With Tidal's cyclic notion of time, in practice this results in *rotating* a pattern. The following pattern shows a quarter rotation, every third cycle:

```{.haskell render="colour" cycles="6"}
every 3 (0.25 <~) $ "red pink grey purple"
```

The `iter` function also shifts time, but keeps shifting it from one cycle to the next, until the cycle returns back to where it started. This takes place over a given number of cycles, which in the following is four:

```{.haskell render="colour" cycles="8"}
iter 4 "red pink grey purple"
```

Lets hear some sound-based examples of `<~` and `iter`:

```{.haskell render="audio" fn="e.wav"}
iter 4 $ sound "bd ht sd cp"
```

```{.haskell render="audio" fn="f.wav"}
every 3 (0.25 <~) $ sound "bd ht sd cp"
```

For a complete introduction to the the pattern transformations available in Tidal, please refer to the website [http://tidalcycles.org](http://tidalcycles.org).

## Patterning multiple dimensions of sound

In terms of how it is perceived, sound is multi-dimensional. A modern synthesiser may have a keyboard providing a pitch dimension, but will also have a plethora of knobs and sliders for exploring further timbral dimensions. Accordingly, Tidal allows different aspects of sound to be patterned independently. The following example demonstrates independent patterning of the `legato` (relative duration) and `lpf` (low pass filter) parameters, where the structure being defined by the `n` (note) rather than `sound` parameter.

```{.haskell render="audio" fn="g.wav"}
off 0.125 (|+| n "-12") $
  jux rev $ n (off "<1%8 1%4>" (+ chord "<major minor major>")
               (palindrome $ "<c(3,8) f(3,8) g(3,8) c6(3,8)>"))
  # sound "superpiano"
  # legato (scale 0.5 2 saw)
  # lpf "<300 [1000 700] 2000>"
  # lpq 0.2
```


<!--
Local Variables:
eval: (visual-line-mode)
End:
-->
