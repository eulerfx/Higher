namespace Higher.Examples

open Higher.Core

/// Base functor for a list.
type ListF<'f, 'a> = Nil | Cons of 'f * 'a

type ListF private () =  
  static let token = new ListF()  
  static member Inj (value : ListF<'f, 'a>) : App2<ListF, 'f, 'a> =
    let app = new App<ListF, 'f>(token, value)
    new App2<ListF, 'f, 'a>(AppToken<ListF, 'f>.Token token, app)  
  static member Prj (app2 : App2<ListF, 'f, 'a>) : ListF<'f, 'a> =
    let app = app2.Apply(AppToken<ListF, 'f>.Token token) :?> App<ListF, 'a>
    app.Apply(token) :?> _

type ListFunctor<'f>() =
  inherit Functor<App<ListF, 'f>>()
    override __.Map (f:'a -> 'b) (l:App2<ListF, 'f, 'a>) : App2<ListF, 'f, 'b> =
      match l |> ListF.Prj with
      | Nil -> Nil |> ListF.Inj
      | Cons (tl,a) -> Cons(tl, f a) |> ListF.Inj     
    
type List<'a> = App<Fix, App<ListF, 'a>>

module List =

  let inline cata (f:ListF<'f, 'b> -> 'b) (l:List<'a>) : 'b =
    Fix.cata 
      (new ListFunctor<_>())
      (fun fa -> fa |> ListF.Prj |> f) 
      (Fix.Prj l)
    
  let length (l:List<'a>) : int =
    l |> cata (function
      | Nil -> 0
      | Cons (tl,a) -> a + 1
    )

//  let length (l:List<'a>) : int =
//    Fix.cata 
//      (new ListFunctor<_>())
//      (fun fa ->        
//        let x = fa |> ListF.Prj
//        match fa |> ListF.Prj with
//        | Nil -> 0
//        | Cons (tl,a) -> a + 1
//      ) 
//      (Fix.Prj l)

  







