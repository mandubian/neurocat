## Neurocat

Neurocat is an experimental toy library studying 2 things:
- **The link between category theory and supervisable learning algorithm and neural networks** through the concepts described in this amazing paper [Backprop as Functor: A compositional
perspective on supervised learning](https://arxiv.org/pdf/1711.10455.pdf?utm_content=bufferf2efc&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer) which tries to unify (partly at least) category theory & supervised learning concepts & simple neural networks/

- **How to represent matrices (and thus neural networks) which dimensions are checked at compile-time using the new feature singleton-types** described in [SIP-23](http://docs.scala-lang.org/sips/pending/42.type.html) which allows to manipulate the integer `3` as the value `3` but also the type `3`. `Shapeless Nat` is a nice idea but not good for big values because this is a recursive structure (down to 0) checked at compile-time so `100000` isn't imaginable with it. My matrix experimentations use the little & really cool library [singleton-ops](https://github.com/fthomas/singleton-ops) by [Frank S. Thomas](https://github.com/fthomas) the author of the great `refined` library too. 

- **how to represent matrices (and thus neural networks) which dimensions are checked at compile-time using the new feature singleton-types** described in [SIP-23](http://docs.scala-lang.org/sips/pending/42.type.html) which allows to manipulate the integer `3` as the value `3` but also the type `3`. My matrix experimentations use the little & really cool library [singleton-ops](https://github.com/fthomas/singleton-ops) by [Frank S. Thomas](https://github.com/fthomas) the author of the great `refined` library too. Shapeless `Nat` is a nice idea but not good for big naturals because this is a recursive structure (down to 0) checked at compile-time so `100000` as 

Very superficially, the idea of the paper is quite simple (minus a few details): 
- A supervised learning algorithm can be seen as a structure able to approximate a function `A -> B` relying on parameters P which are updated through an optimization/training process using a set of training samples.
- This paper shows that the set of supervised learning algorithms equipped with 3 functions (implement, update-params, request-input) forms a **symmetric monoidal category `Learn`** and then demonstrates that **supervised learning algorithms can be composed**
- It also shows that there exists **a Functor from the category `ParaFn` of parametrised functions `P -> A -> B` to `Learn` category**.

```ParaFn -> Learn```

- Then it shows that a (trained) neural network can be seen as an approximation of a function `InputLayer -> OutputLayer` parametrised by the weights W.
- Thus it demonstrates there is also **a Functor from the category of neural network (W, InputLayer, OutputLayer) to the category of parametrised functions (W -> InputLayer -> OutputLayer)**

```I: NNet -> ParaFn```

- By simple functor composition, you have then a Functor from neural networks to supervised learning algorithms:

```NNet -> Learn : (ParaFn -> Learn) ∘ (NNet -> ParaFn)```

> I'll stop there for now but my work has just started and there are more concepts about the bimonoidal aspects of neural networks under euclidean space constraints and pending studies about recurrent networks and more.


Discovering that formulation, I just said: "Whoaaa that's cool, exactly what I had in mind without being able to put words on it".
Why? Because everything I've seen about neural networks looks like programming from the 70s, not like I program nowadays with Functional Programming, types & categories.
This starts unifying concepts and is exactly the reason of being of category theory in maths. I think programming learning algorithms will change a lot in the future exactly as programming backends changed a lot those last 10 years.

I'm just scratching the surface of all of those concepts. I'm not a NeuralNetwork expert at all neither a good mathematician so I just want to open this field of study in a language which now has singleton-types allowing really cool new ways of manipulating data structures

So first, have a look at this sample:

- Basic Compile-time Matrix calculus: https://github.com/mandubian/neurocat/blob/master/src/test/scala/MatTest.scala#L15-L47
- Neural network layers transformed into Learn instances and then composed and trained: https://github.com/mandubian/neurocat/blob/master/src/test/scala/NNetTest.scala#L40-L129
- Neural network + Huge Matrices that compiles in human time: https://github.com/mandubian/neurocat/blob/master/src/test/scala/NNetTest.scala#L131-L163

> For info, to manipulate matrices, I used [ND4J](http://nd4j.org/) to have an array abstraction to test both in CPU or GPU mode but any library doing this could be used naturally.

