﻿namespace Higher.Core
open System 

[<AbstractClass>]
type Semigroup<'T>() =
    abstract Append : 'T -> 'T -> 'T

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Semigroup =
  
  let ofAppend (f:'T -> 'T -> 'T) : Semigroup<'T> =
    { new Semigroup<'T>() with override __.Append  a b = f a b }
  

// Monoid class
[<AbstractClass>]
type Monoid<'T>() =
    inherit Semigroup<'T>()
    abstract Empty : 'T
    
type Monoid private () =
  static let token = Monoid()
  static member Inj (m : 'T) : App2<Monoid, 'A, 'B> =
    let app = new App<Monoid, 'A>(token, m)
    new App2<Monoid, 'A, 'B>(AppToken<Monoid, 'A>.Token token, app)
  static member Prj (app2: App2<Monoid, 'A, 'B>) : 'T =
    let app = app2.Apply(AppToken<Monoid, 'A>.Token token) :?> App<Monoid, 'A>
    app.Apply(token) :?> _
  static member Run (app2: App2<Monoid, 'A, 'B>) : 'T =
    Monoid.Prj app2

// Monoids are single object categories
type MonoidCategory<'T>(m: Monoid<'T>) =
  inherit Category<Monoid>() with    
    override self.Ident<'A> () : App2<Monoid, 'A, 'A> =
      Monoid.Inj m.Empty
    override self.Compose<'A, 'B, 'C> (ab:App2<Monoid, 'A, 'B>) (bc:App2<Monoid, 'B, 'C>) : App2<Monoid, 'A, 'C> =
      m.Append (Monoid.Prj ab) (Monoid.Prj bc) |> Monoid.Inj

    
// Basic Monoid instances
type StringMonoid() =
    inherit Monoid<string>() with 
    override self.Empty = ""  
    override self.Append x y = x + y

type ListMonoid<'T>() =
    inherit Monoid<'T list>() with 
    override self.Empty = []  
    override self.Append xs ys = List.append xs ys

type SeqMonoid<'T>() =
    inherit Monoid<seq<'T>>() with 
    override self.Empty = Seq.empty   
    override self.Append xs ys = Seq.append xs ys

type EndoMonoid<'T>() =
    inherit Monoid<'T -> 'T>() with
    override self.Empty = id
    override self.Append f g = f >> g

type IntSumMonoid() =
    inherit Monoid<int>() with
    override self.Empty = 0
    override self.Append f g = f + g