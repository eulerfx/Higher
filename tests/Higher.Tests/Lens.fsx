#r "bin/release/Higher.Core.dll"

open Higher.Core

type Const<'A, 'B> = Const of 'A

type Const private () =  
  static let token = Const ()
  static member inline un (Const(a)) = a
  static member Inj (value : Const<'A, 'B>) : App2<Const, 'A, 'B> =
    App2<Const, 'A, 'B>(AppToken<Const, 'A>.Token token, value)
  static member Prj (app : App2<Const, 'A, 'B>) : Const<'A, 'B> =
    app.Apply(AppToken<Const, 'A>.Token token) :?> _


type ConstFunctor<'M> () =
  inherit Functor<App<Const, 'M>> () with
    override self.Map (_:'A -> 'B) (app:App2<Const, 'M, 'A>) : App2<Const, 'M, 'B> =
      app |> Const.Prj |> Const.un |> Const.Const |> Const.Inj




type Lens = Lens

/// A traversal is a lens where the functor is an applicative.
type Traversal<'a, 'b, 'F> = ('b -> App<'F, 'b>) -> ('a -> App<'F, 'a>)

type Lens<'a, 'b, 'F> = ('b -> App<'F, 'b>) -> ('a -> App<'F, 'a>)

type ILens<'a, 'b> = 
  abstract member Apply<'F> : Functor<'F> * ('b -> App<'F, 'b>) * 'a -> App<'F, 'a>


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =
  
  let compose2 (bc:ILens<'b, 'c>) (ab:ILens<'a, 'b>) : ILens<'a, 'c> =
    { new ILens<_, _> with
        override __.Apply<'F> (F, f:'c -> App<'F, 'c>, a:'a) : App<'F, 'a> =
          ab.Apply(F, (fun b -> bc.Apply(F, f, b)), a) }

  let id2<'a> : ILens<'a, 'a> =
    { new ILens<_, _> with
        override __.Apply<'F> (F, f, a:'a) : App<'F, 'a> = f a }

  let get (l:Lens<'a, 'b, _>) : 'a -> 'b =
    l (Const.Const >> Const.Inj) >> Const.Prj >> Const.un

  let get2 (l:ILens<'a, 'b>) : 'a -> 'b =
    fun a -> l.Apply ((new ConstFunctor<_>()), (Const.Const >> Const.Inj), a) |> Const.Prj |> Const.un

  let modify (l:Lens<'a, 'b, _>) (m:'b -> 'b) : 'a -> 'a =
    l (m >> Id >> Identity.Inj) >> Identity.Prj >> Identity.un

  let modify2 (l:ILens<'a, 'b>) : 'a -> 'a =
    fun a -> l.Apply ((new IdentityFunctor()), (Identity.Id >> Identity.Inj), a) |> Identity.Prj |> Identity.un

  let set (l:Lens<'a, 'b, _>) (b:'b) : 'a -> 'a =
    modify l (fun _ -> b)
  
  let fst (F:Functor<'F>) : Lens<'a * 'b, 'a, 'F> =
    fun f (a,b) -> f a |> F.Map (fun a' -> a',b)

  let fst2<'a, 'b> : ILens<'a * 'b, 'a> =
    { new ILens<_, _> with
        override __.Apply (F, f, (a,b)) =
          f a |> F.Map (fun a' -> a',b) }


  let id (F:Functor<'F>) : Lens<'a, 'a, 'F> =
    Operators.id

  let compose (bc:Lens<'b, 'c, 'F>) (ab:Lens<'a, 'b, 'F>) : Lens<'a, 'c, 'F> =
    ab << bc



