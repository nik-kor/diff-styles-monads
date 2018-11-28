# Different monad styles

It is a study project - kind of homework for the great workshop by [@serras](https://github.com/serras)
and [the slides](https://es.slideshare.net/AlejandroMena6/build-your-own-monads).

My motivation was to explore Final style(MTL) and Initial style(Free) monads and build a toy but a working program with them.
I took tic-tac-toe game as the most straightforward possible program to implement and also all the examples from the
workshop were for it. Btw it could be not the best choice - it might be that an example was too simple and the whole
solution was overengineered.


## Some observations

* I loved the approach from the workshop - define primitives, build API on top, implement interpretations. Though it
was a bit misleading where should I start - e.g., it wasn't clear which primitives should I introduce, do we need
higher-level API, could we use primitives in the monadic calculations, etc. It probably can be clearer with refactoring
an existing project.
* [quite subjective] Looks like Free style is more popular in Scala community - maybe because John A De Goes was
pushing for it and MTL style is preferable in Haskell world. My impression is based on the content about the topic I could find.
* MTL style seems easier
 - I could combine monads
 - it's just a type classes at the end
* It seems like Free style is more elegant but harder to master - e.g., I couldn't figure out how to combine functors.
* http://hackage.haskell.org/package/freer-effects worth looking

## Project structure

There are 3 packages:

* free/ - meat for Free style monads implementation
* mtl/ - for MTL style
* common/ - to share common data structures

## What I couldn't do

* I didn't get how to share the same functions between different approaches - the code is the same, but the types are
a little bit different. Probably, there is a way of doing it, but it could for good because the both
apps kind of isolated and not dependant on each other

* I tried to use "Data types à la carte" approach and the http://hackage.haskell.org/package/compdata to compose functors in
`free` version. However, I couldn't build smart constructors because of the type conflicts - e.g.
```
type TicTacToeM = Free (TicTacToeGameF :+: TicTacToeRenderF)

info :: Position -> TicTacToeM (Maybe Player)
info p = Free (Info p return)


    • Couldn't match type ‘TicTacToeGameF’
                     with ‘TicTacToeGameF :+: TicTacToeRenderF’
      Expected type: TicTacToeM Moves
        Actual type: Free TicTacToeGameF Moves
    • In the expression: Free (GetMoves return)
      In an equation for ‘moves’: moves = Free (GetMoves return)
```

* Still not sure if I'm using stack `packages` correctly

## Possible improvements

* nicer API - at the end there should be identical `app`(mtl) and `game`(free) monads
* test `app` in mtl version


## Other resources

* https://markkarpov.com/post/free-monad-considered-harmful.html
* https://github.com/ekmett/free
* https://mmhaskell.com/blog/2017/11/20/eff-to-the-rescue
* https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html
* http://degoes.net/articles/modern-fp
* https://www.youtube.com/watch?v=gUPuWHAt6SA&t=2191s
