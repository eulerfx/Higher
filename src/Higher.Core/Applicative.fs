namespace Higher.Core

// Applicative Class 
[<AbstractClass>]
type Applicative<'F>() = 
    inherit Functor<'F>() 
    override self.Map f func = 
        self.Apply (self.Pure f) func
    abstract Pure<'T> : 'T -> App<'F, 'T>
    abstract Apply<'T, 'R> : App<'F, 'T -> 'R> -> App<'F, 'T> -> App<'F, 'R>

module Applicative =
  
  let compose (F:Applicative<'F>) (G:Applicative<'G>) : Applicative<Compose<'F, 'G>> =    
    { new Applicative<_>() with
        member x.Pure<'A> (a:'A) : Compose<'F, 'G, 'A> = 
          let a = F.Pure (G.Pure a)
          Compose.Inj a
        member x.Apply (f:Compose<'F, 'G, 'A -> 'B>) (fa:Compose<'F, 'G, 'A>) : Compose<'F, 'G, 'B> = 
          let f' = Compose.Prj f
          let fa' = Compose.Prj fa
          F.Apply (F.Map (G.Apply) f') fa' |> Compose.Inj }
            
