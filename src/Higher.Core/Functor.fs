namespace Higher.Core

// Functor base classes 

[<AbstractClass>]
type Functor<'F>() = 
    abstract Map<'A, 'B> : ('A -> 'B) -> App<'F, 'A> -> App<'F, 'B>

[<AbstractClass>]
type ContraFunctor<'F>() = 
    abstract ContraMap<'A, 'B> : ('A -> 'B) -> App<'F, 'B> -> App<'F, 'A>

[<AbstractClass>]
type BiFunctor<'F>() = 
    abstract BiMap<'A, 'B, 'C, 'D> : ('A -> 'B) -> ('C -> 'D) -> App2<'F, 'A, 'C> -> App2<'F, 'B, 'D>
    member self.First<'A, 'B, 'C> (f : 'A -> 'B) (fac : App2<'F, 'A, 'C>) : App2<'F, 'B, 'C> = 
        self.BiMap f id fac 
    member self.Second<'A, 'B, 'C> (f : 'B -> 'C) (fab : App2<'F, 'A, 'B>) : App2<'F, 'A, 'C> = 
        self.BiMap id f fab

[<AbstractClass>]
type ProFunctor<'F>() =
    abstract DiMap<'A, 'B, 'C, 'D> : ('C -> 'A) -> ('B -> 'D) -> App2<'F, 'A, 'B> -> App2<'F, 'C, 'D>
    member self.First<'A, 'B, 'C> (f : 'C -> 'A) (fab : App2<'F, 'A, 'B>) : App2<'F, 'C, 'B> =
        self.DiMap f id fab
    member self.Second<'A, 'B, 'D> (f : 'B -> 'D) (fab : App2<'F, 'A, 'B>) : App2<'F, 'A, 'D> =
        self.DiMap id f fab


module Functor =
  
  let compose (F:Functor<'F>) (G:Functor<'G>) : Functor<Comp<'F, 'G>> =
    { new Functor<_>() with
        member __.Map<'A, 'B> (f:'A -> 'B) (fga:App<Comp<'F, 'G>, 'A>) : App<Comp<'F, 'G>, 'B> =
          F.Map (G.Map f) (Comp.Prj fga) |> Comp.Inj }
        
module FunctorLaws =
  
  let identity (eq : App<'F, 'A> -> App<'F, 'A> -> bool) (func : Functor<'F>) (fa : App<'F, 'A>) =
    eq (func.Map id fa) fa

module BiFunctorLaws =
  
  let identity (eq : App2<'F, 'A, 'B> -> App2<'F, 'A, 'B> -> bool) (func : BiFunctor<'F>) (fab : App2<'F, 'A, 'B>) =
    eq (func.BiMap id id fab) fab

module ProFunctorLaws =
  
  let identity (eq : App2<'F, 'A, 'B> -> App2<'F, 'A, 'B> -> bool) (func : ProFunctor<'F>) (fab : App2<'F, 'A, 'B>) =
    eq (func.DiMap id id fab) fab  

   



module NatComp =
  
  /// Witnesses that natural transformations can be composed vertically.
  let vert (F2:Functor<'F2>) : Nat<'F2, 'G2> -> Nat<'F, 'G> -> NatComp<'F, 'F2, 'G, 'G2> =
    fun (a:Nat<'F2, 'G2>) (b:Nat<'F, 'G>) ->
      { new NatComp<_,_,_,_> with
          member __.NatComp (x:App<Comp<'F2, 'F>, 'A>) : App<Comp<'G2, 'G>, 'A> =
            a.Nat (F2.Map b.Nat (Comp.Prj x)) |> Comp.Inj }

  /// Witnesses that natural transformations can be composed horizontally.
  let horiz (G2:Functor<'G2>) : Nat<'F2, 'G2> -> Nat<'F, 'G> -> NatComp<'F, 'F2, 'G, 'G2> =
    fun (a:Nat<'F2, 'G2>) (b:Nat<'F, 'G>) ->
      { new NatComp<_,_,_,_> with
          member __.NatComp (x:App<Comp<'F2, 'F>, 'A>) : App<Comp<'G2, 'G>, 'A> =
            G2.Map b.Nat (a.Nat (Comp.Prj x)) |> Comp.Inj }