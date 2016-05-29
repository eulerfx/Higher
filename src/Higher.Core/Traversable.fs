namespace Higher.Core
               
// Traversable Class
[<AbstractClass>]
type Traversable<'T>() = 
    inherit Functor<'T>()     
    abstract Traverse<'F, 'A, 'R> : Applicative<'F> -> ('A -> App<'F, 'R>) -> App<'T, 'A> -> App<'F, App<'T, 'R>>
    member self.SequenceA applicative trav =
        self.Traverse applicative id trav 
    override self.Map f fa : App<'T, 'B> =
      self.Traverse (new IdentityApplicative()) (f >> Identity.Id >> Identity.Inj) fa |> Identity.Prj |> Identity.un
    member self.FoldMapDefault (M:Monoid<'M>) (f:'A -> 'M) : App<'T, 'A> -> 'M =
      self.Traverse (new ConstApplicative<'M> (M)) (f >> Const.Const >> Const.Inj) >> Const.Prj >> Const.un
    

