#r "bin/release/Higher.Core.dll"

open Higher.Core

type Ran<'G, 'H, 'A> =
  abstract member Apply<'B> : ('A -> App<'G, 'B>) -> App<'H, 'B>

type Lan<'G, 'H, 'A> =
  abstract member Apply<'B> : (App<'G, 'B> -> 'A) -> App<'H, 'B> -> Lan<'G, 'H, 'A>


[<AbstractClass>]
type Distributive<'G>() = 
  inherit Functor<'G>()     
  abstract Distribute<'F, 'A> : Functor<'F> -> App<'F, App<'G, 'A>> -> App<'G, App<'F, 'A>>
  abstract Collect<'F, 'A, 'B> : Functor<'F> -> ('A -> App<'G, 'B>) -> App<'F, 'A> -> App<'G, App<'F, 'B>>


[<AbstractClass>]
type Rep<'F>() =
  inherit Distributive<'F>()
  abstract member Tabulate : (Rep<'F> -> 'A) -> App<'F, 'A>
  abstract member Index : App<'F, 'A> -> Rep<'F> -> 'A


type Adjunction<'F, 'U> =

  abstract member Unit<'A> : 'A -> App<'U, App<'F, 'A>>
  abstract member CoUnit<'A> : App<'F, App<'U, 'A>> -> 'A

  abstract member LeftAdjunct<'A, 'B> : (App<'F, 'A> -> 'B) -> 'A -> App<'U, 'B>
  abstract member RightAdjunct<'A, 'B> : ('A -> App<'U, 'B>) -> App<'F, 'A> -> 'B



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




