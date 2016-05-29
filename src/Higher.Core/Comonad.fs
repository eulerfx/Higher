﻿namespace Higher.Core

// Comonad Class 
[<AbstractClass>]
type Comonad<'W>() = 
    inherit Functor<'W>()
    override self.Map (f : 'A -> 'B) (func : App<'W, 'A>) =
      self.Extend (self.Extract >> f) func
    abstract Extract<'A> : App<'W, 'A> -> 'A
    abstract Extend<'A, 'B> : (App<'W, 'A> -> 'B) -> App<'W, 'A> -> App<'W, 'B>

type IdentityComonad() =
  inherit Comonad<Identity>() with
    override self.Extract (app : App<Identity, 'A>) : 'A =
      app |> Identity.Prj |> Identity.un
    override self.Extend (f : App<Identity, 'A> -> 'B) (app : App<Identity, 'A>) : App<Identity, 'B> =
      app |> f |> Id |> Identity.Inj

// Generic comonad functions.
module Comonad =
  
  let duplicate (comonad : Comonad<'W>) (w : App<'W, 'A>) : App<'W, App<'W, 'A>> =
    comonad.Extend id w

  let rec fix (comonad : Comonad<'W>) (w : App<'W, App<'W, 'A> -> 'A>) : 'A =
    comonad.Extend (fix comonad) w |> comonad.Extract


[<AbstractClass>]
type ComonadZip<'W>() =
  inherit Comonad<'W>()
    abstract Apply : App<'W, 'A -> 'B> -> App<'W, 'A> -> App<'W, 'B>

module ComonadZip =
  
  let lift2 (comonad : ComonadZip<'W>) (f : 'A -> 'B -> 'C) (wa : App<'W, 'A>) (wb : App<'W, 'B>) : App<'W, 'C> =
    let wbc = comonad.Extend (comonad.Extract >> f) wa
    comonad.Apply wbc wb

  let zip (comonad : ComonadZip<'W>) (wa : App<'W, 'A>) (wb : App<'W, 'B>) : App<'W, 'A * 'B> =
    lift2 comonad (fun a b -> a,b) wa wb
    
    