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
  
  let compose (F:Applicative<'F>) (G:Applicative<'G>) : Applicative<Comp<'F, 'G>> =    
    { new Applicative<_>() with
        member x.Pure<'A> (a:'A) : App<Comp<'F, 'G>, 'A> = 
          let a = F.Pure (G.Pure a)
          Comp.Inj a
        member x.Apply (f:App<Comp<'F, 'G>, 'A -> 'B>) (fa:App<Comp<'F, 'G>, 'A>) : App<Comp<'F, 'G>, 'B> = 
          let f' = Comp.Prj f
          let fa' = Comp.Prj fa
          F.Apply (F.Map (G.Apply) f') fa' |> Comp.Inj }
            
