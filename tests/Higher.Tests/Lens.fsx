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



type Lens = Lens

/// A traversal is a lens where the functor is an applicative.
type Traversal<'a, 'b, 'F> = ('b -> App<'F, 'b>) -> ('a -> App<'F, 'a>)

type Lens<'a, 'b, 'F> = ('b -> App<'F, 'b>) -> ('a -> App<'F, 'a>)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Lens =
  
  let get (l:Lens<'a, 'b, _>) : 'a -> 'b =
    l (Const.Const >> Const.Inj) >> Const.Prj >> Const.un

  let modify (l:Lens<'a, 'b, _>) (m:'b -> 'b) : 'a -> 'a =
    l (m >> Id >> Identity.Inj) >> Identity.Prj >> Identity.un

  let set (l:Lens<'a, 'b, _>) (b:'b) : 'a -> 'a =
    modify l (fun _ -> b)
  
  let fst (F:Functor<'F>) : Lens<'a * 'b, 'a, 'F> =
    fun f (a,b) -> f a |> F.Map (fun a' -> a',b)

  let id (F:Functor<'F>) : Lens<'a, 'a, 'F> =
    Operators.id

  let compose (bc:Lens<'b, 'c, 'F>) (ab:Lens<'a, 'b, 'F>) : Lens<'a, 'c, 'F> =
    ab << bc
