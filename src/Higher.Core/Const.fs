namespace Higher.Core

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

type ConstApplicative<'M> (M:Monoid<'M>) =
  inherit Applicative<App<Const, 'M>> () with
    override self.Pure (a:'A) : App2<Const, 'M, 'A> =
      M.Empty |> Const.Const |> Const.Inj
    override self.Apply (f:App2<Const, 'M, 'A -> 'B>) (fa:App2<Const, 'M, 'A>) : App2<Const, 'M, 'B> =
      M.Append (Const.Prj f |> Const.un) (Const.Prj fa |> Const.un) |> Const.Const |> Const.Inj

