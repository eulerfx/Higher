namespace Higher.Core
open System

// The basic idea of Type Defunctionalization is based on 
// https://ocamllabs.github.io/higher/lightweight-higher-kinded-polymorphism.pdf
// OCaml implementation https://github.com/ocamllabs/higher

// Represents type application
// To ensure type-safety we use a secret token based control access policy.
type App<'F, 'T> (token : 'F, value : obj) =
    do 
        if Object.ReferenceEquals(token,  Unchecked.defaultof<'F>) then
            raise <| new System.InvalidOperationException("Invalid token")
    // Apply the secret token to have access to the encapsulated value
    member self.Apply(token' : 'F) =
        if Object.ReferenceEquals(token, token') then  
            value 
        else raise <| new InvalidOperationException("Invalid token")

type App2<'F, 'T1, 'T2> = App<App<'F, 'T1>, 'T2>
type App3<'F, 'T1, 'T2, 'T3> = App<App2<'F, 'T1, 'T2>, 'T3>
type App4<'F, 'T1, 'T2, 'T3, 'T4> = App<App3<'F, 'T1, 'T2, 'T3>, 'T4>

// A Singleton-like type for managing parameterized tokens 
type AppToken<'App, 'R>() = 
    static let appTokenRef = ref Unchecked.defaultof<App<'App, 'R>> 
    static member Token (token : 'App) = 
        if !appTokenRef = Unchecked.defaultof<App<'App, 'R>> then
            lock appTokenRef (fun () ->
                if !appTokenRef = Unchecked.defaultof<App<'App, 'R>> then
                    appTokenRef := new App<'App, 'R>(token, Unchecked.defaultof<'R>)
            )
        !appTokenRef


/// Composition of 'F and 'G.
type Comp<'F, 'G> = App2<Comp, 'F, 'G>

and Comp private () =
  static let token = new Comp ()
  static member Inj (value:App<'F, App<'G, 'A>>) : App<Comp<'F, 'G>, 'A> =
    let app = App<_, _>(token, value)
    let app2 = App2<_, _, _>(AppToken<Comp, 'F>.Token token, app)
    let token2 : App<Comp, 'F> = AppToken<Comp, 'F>.Token token
    App3<_, _, _, _>(AppToken<App<Comp, 'F>, 'G>.Token token2, app2)  
  static member Prj (app3:App<Comp<'F, 'G>, 'A>) : App<'F, App<'G, 'A>> =
    let t = AppToken<Comp, 'F>.Token token
    let t1 : App2<Comp, 'F, 'G> = AppToken<_, _>.Token t
    let app2 : App2<Comp, 'F, 'G> = app3.Apply t1 :?> _
    let app : App<Comp, 'F> = app2.Apply t :?> _
    app.Apply token :?> _


/// Natural transformation between 'F and 'G.
type Nat<'F, 'G> =
  abstract member Nat<'A> : App<'F, 'A> -> App<'G, 'A>


/// 
type NatComp<'F, 'F2,  'G, 'G2> =
  abstract member NatComp<'A> : App<Comp<'F2, 'F>, 'A> -> App<Comp<'G2, 'G>, 'A>




[<AutoOpen>]
module Prelude =
  
  let undef<'a> : 'a = failwith "undefined"
