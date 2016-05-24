namespace Higher.Core

// Applicative Class 
[<AbstractClass>]
type Applicative<'F>() = 
    inherit Functor<'F>() 
    override self.Map f func = 
        self.Apply (self.Pure f) func
    abstract Pure<'T> : 'T -> App<'F, 'T>
    abstract Apply<'T, 'R> : App<'F, 'T -> 'R> -> App<'F, 'T> -> App<'F, 'R>

type Compose<'f, 'g, 'a> = App<'f, App<'g, 'a>>

module Applicative =
  
  // f (g a)
  let compose (F:Applicative<'F>) (G:Applicative<'G>) : Applicative<App<'F, 'G>> =    
    { new Applicative<App<'F, 'G>>() with
        member x.Pure<'T> (a:'T) = 
          //let ga = G.Pure a in
          //let a = F.Pure (G.Pure a)
          let x = failwith "" in x
        member x.Apply (f) (a) = failwith "" }
            
