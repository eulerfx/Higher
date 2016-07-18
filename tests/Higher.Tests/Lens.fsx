#r "bin/release/Higher.Core.dll"

open Higher.Core

/// A polymorphic function.
type Nat<'F, 'G> = 
  abstract member Apply<'A> : App<'F, 'A> -> App<'G, 'A>


type HeadOption () =
  interface Nat<List, Option> with
    override __.Apply<'A> (ls:App<List, 'A>) : App<Option, 'A> =
      ls |> List.Prj |> List.tryHead |> Option.Inj


type IdNat () =
  interface Nat<Identity, Identity> with
    override __.Apply<'A> (x:App<Identity, 'A>) : App<Identity, 'A> =
      x |> Identity.Prj |> Identity.un |> Identity.Id |> Identity.Inj


type Ran<'G, 'H, 'A> =
  abstract member Apply<'B> : ('A -> App<'G, 'B>) -> App<'H, 'B>

type Codensity<'M, 'A> = Ran<'M, 'M, 'A>

type Ran = Ran
  with
    static member Prj (app3:App3<Ran, 'G, 'H, 'A>) : Ran<'G, 'H, 'A> = 
      failwith ""  
    static member Inj (value:Ran<'G, 'H, 'A>) : App3<Ran, 'G, 'H, 'A> = 
      failwith ""

type Lan<'G, 'H, 'A> =
  abstract member Apply<'B> : (App<'G, 'B> -> 'A) -> App<'H, 'B> -> Lan<'G, 'H, 'A>

type DiNat<'F, 'G> =
  abstract member Apply<'A> : App2<'F, 'A, 'A> -> App2<'G, 'A, 'A>

type End<'S> =
  abstract member Apply<'A> : unit -> App2<'S, 'A, 'A>

module Kan =
  
  let ranFunctor<'G, 'H> : Functor<App2<Ran, 'G, 'H>> =
    { new Functor<_>() with
        member __.Map<'A, 'B> (f:'A -> 'B) (r:App3<Ran, 'G, 'H, 'A>) : App3<Ran, 'G, 'H, 'B> =
          { new Ran<'G, 'H, 'B> with
              member __.Apply<'C> (k:'B -> App<'G, 'C>) : App<'H, 'C> =
                let x : Ran<'G, 'H, 'A> = Ran.Prj r
                x.Apply (f >> k) } |> Ran.Inj }


[<AbstractClass>]
type Distributive<'G>() = 
  inherit Functor<'G>()     
  abstract Distribute<'F, 'A> : Functor<'F> -> App<'F, App<'G, 'A>> -> App<'G, App<'F, 'A>>
  abstract Collect<'F, 'A, 'B> : Functor<'F> -> ('A -> App<'G, 'B>) -> App<'F, 'A> -> App<'G, App<'F, 'B>>

[<AbstractClass>]
type Adjunction<'F, 'G> =
  abstract member Unit<'A> : 'A -> App<'G, App<'F, 'A>>
  abstract member CoUnit<'A> : App<'F, App<'G, 'A>> -> 'A
  member self.LeftAdjunct<'A, 'B> (G:Functor<'G>) (f:(App<'F, 'A> -> 'B)) : ('A -> App<'G, 'B>) =
    self.Unit >> G.Map f                 
  member self.RightAdjunct<'A, 'B> (F:Functor<'F>) (f:('A -> App<'G, 'B>)) : App<'F, 'A> -> 'B =
    F.Map f >> self.CoUnit
    
module Adjunction = 

  let monad (adj:Adjunction<'G, 'F>) (F:Functor<'F>) (G:Functor<'G>) : Monad<Comp<'F, 'G>> =
    { new Monad<_>() with
        member __.Return<'A> (a:'A) : App<Comp<'F, 'G>, 'A> =
          adj.Unit a |> Comp.Inj
        member __.Bind<'A, 'B> (a:App<Comp<'F, 'G>, 'A>, f:'A -> App<Comp<'F, 'G>, 'B>) : App<Comp<'F, 'G>, 'B> =
          F.Map (adj.RightAdjunct G (f >> Comp.Prj)) (Comp.Prj a) |> Comp.Inj }

  let comonad (adj:Adjunction<'F, 'G>) (F:Functor<'F>) (G:Functor<'G>) : Comonad<Comp<'F, 'G>> =
    { new Comonad<_>() with
        member __.Extract<'A> (w:App<Comp<'F, 'G>, 'A>) : 'A =
          adj.CoUnit (Comp.Prj w)
        member __.Extend<'A, 'B> (f:App<Comp<'F, 'G>, 'A> -> 'B) (w:App<Comp<'F, 'G>, 'A>) : App<Comp<'F, 'G>, 'B> =
          F.Map (adj.LeftAdjunct G (Comp.Inj >> f)) (Comp.Prj w) |> Comp.Inj }


type Lens = Lens

/// A polymorphic lens.
/// - 's - source
/// - 't - target
/// - 'a - source value
/// - 'b - target value
type Lens<'s, 't, 'a, 'b> = 
  abstract member Apply<'F> : Functor<'F> -> ('a -> App<'F, 'b>) -> ('s -> App<'F, 't>)

/// A lens.
type Lens<'a, 'b> = Lens<'a, 'a, 'b, 'b>

type Fold<'a, 'b> = 
  abstract member Apply<'M> : Monoid<'M> -> ('b -> Const<'M, 'b>) -> ('a -> Const<'M, 'a>)

/// A traversal is a lens where the functor is applicative.
type Traversal<'a, 'b> = 
  abstract member Apply<'F> : Applicative<'F> -> ('b -> App<'F, 'b>) -> ('a -> App<'F, 'a>)

type Setter<'a, 'b> = ('a -> Identity<'a>) -> ('b -> Identity<'b>)

type Getting<'r, 'a, 'b> = ('a -> Const<'r, 'a>) -> ('b -> Const<'r, 'b>)

/// Operations on lenses.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =

  let lens (get:'s -> 'a) (set:'s -> 'b -> 't) : Lens<'s, 't, 'a, 'b> =
    { new Lens<_,_,_,_> with
        override __.Apply<'F> F (f:'a -> App<'F, 'b>) : 's -> App<'F, 't> =
          fun s -> f (get s) |> F.Map (fun b -> set s b) }                   
  
  let compose (bc:Lens<'b, 'c>) (ab:Lens<'a, 'b>) : Lens<'a, 'c> =
    { new Lens<_, _> with
        override __.Apply<'F> F (f:'c -> App<'F, 'c>) : 'a -> App<'F, 'a> =
          ab.Apply F (bc.Apply F f) }

  let id<'a> : Lens<'a, 'a, 'a, 'a> =
    { new Lens<_, _> with
        override __.Apply<'F> F f : 'a -> App<'F, 'a> = f }

  let get (l:Lens<'s, 't, 'a, 'b>) : 's -> 'a =
    l.Apply (new ConstFunctor<_>()) (Const.Const >> Const.Inj) >> Const.Prj >> Const.un

  let modify (l:Lens<'s, 't, 'a, 'b>) (f:'a -> 'b) : 's -> 't =
    l.Apply (new IdentityFunctor()) (fun b -> Identity.Id (f b) |> Identity.Inj) >> Identity.Prj >> Identity.un

  let set (l:Lens<'a, 'b>) (b:'b) : 'a -> 'a =
    modify l (fun _ -> b)
  
  let fst<'a, 'b> : Lens<'a * 'b, 'a> =
    lens fst (fun (_,b) a -> (a,b))

  let snd<'a, 'b> : Lens<'a * 'b, 'b> =
    lens snd (fun (a,_) b -> (a,b))




/// Backtracking.
type BT<'A> =
  abstract member Apply<'Z> : ('A -> 'Z -> 'Z) -> 'Z -> 'Z

module BT =
  
  let fail<'A> : BT<'A> =
    { new BT<_> with
        member __.Apply s f = f }

  let join (a:BT<'A>) (b:BT<'A>) : BT<'A> =
    { new BT<_> with
        member __.Apply s f = a.Apply s (b.Apply s f) }
