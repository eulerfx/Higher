namespace Higher.Core

type Identity<'A> = Id of 'A

type Identity private () =  
  static let token = Identity()
  static member inline un (Id(a)) = a
  static member Inj (value : Identity<'A>) : App<Identity, 'A> =
    App<Identity, 'A>(token, value)
  static member Prj (app : App<Identity, 'A>) : Identity<'A> =
    app.Apply(token) :?> _
    
type IdentityFunctor() =
  inherit Functor<Identity>() with
    override self.Map (f : 'A -> 'B) (app : App<Identity, 'A>) : App<Identity, 'B> =
      app |> Identity.Prj |> Identity.un |> f |> Id |> Identity.Inj

type IdentityApplicative() =
  inherit Applicative<Identity>() with
    override self.Pure (a : 'A) : App<Identity, 'A> =
      Id a |> Identity.Inj
    override self.Apply (fab : App<Identity, 'A -> 'B>) (a : App<Identity, 'A>) : App<Identity, 'B> =
      let f = fab |> Identity.Prj |> Identity.un in
      f (Identity.Prj a |> Identity.un) |> Id |> Identity.Inj

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Identity =
  
  let unId (F:Functor<'F>) (fc:App<Comp<'F, Identity>, 'A>) : App<'F, 'A> =            
    F.Map (Identity.Prj >> Identity.un) (Comp.Prj fc)
  
  let toId (F:Functor<'F>) (fa:App<'F, 'A>) : App<Comp<'F, Identity>, 'A> =
    F.Map (Identity.Id >> Identity.Inj) fa |> Comp.Inj
